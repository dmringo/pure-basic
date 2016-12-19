{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE 
PatternGuards, MultiParamTypeClasses, FunctionalDependencies,
FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
UndecidableInstances, OverloadedStrings, MultiWayIf #-}


module Basic.Eval
  (run, runProg, Eval(..)
  )where

import Basic.Doub hiding (D)
import Basic.AST
import Basic.Type
import Basic.Parser (parseDoubs) -- for INPUT
  
import Data.Char (isNumber)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.List (uncons)  
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M  
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector ((!?))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V (write)

import Text.Printf (printf)
import Text.Show.Pretty  (ppShow, pPrint)

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Extra

import System.Random
import System.IO (hFlush, stdout)  
  
data Val
  = S Text
  | N Doub
  | A Dims (Vec Val)

instance Eq Val where
  S a == S b = a == b
  N a == N b = a == b
  _   == _   = False    

instance TC Val where
  unifies = Yes . typeof
  typeof (S _) = Stringy
  typeof (N _) = Numeric
  typeof (A _ v) = maybe None typeof $ v !? 0

instance Show Val where
  showsPrec p v =
    case v of
      S t -> showString $ T.unpack t
      N d -> showString $ show d
      A dim v -> showString $ show v


emptyString, zero :: Val
emptyString = S T.empty                     
zero        = N 0
              
data VMErr
  = BadSubscript
    PC  -- | On what line did it happen?
    Dims -- | What was the index used?    
    Dims -- | What was the upper (inclusive) limit?
  | BadAddress
    PC  -- | On what line did it happen?
    Int -- | What was the address?
  | BadType
    PC  -- | On what line did it happen?
    Type -- | Expected this
    Type -- | Got this
  | NextError -- | Next without For
    PC -- | On what line did it happen?
  | ForError -- | For without Next
    PC -- | On what line did it happen?
  | WendError -- | Wend without While
    PC -- | On what line did it happen?
  | WhileError -- | While without Wend
    PC -- | On what line did it happen?
  | RetError -- | RETURN without GOSUB
    PC -- | On what line did it happen?

instance Show VMErr where
  showsPrec p e =
    showString $
    case e of
      BadSubscript linum tried real
        -> printf "on line %d ==> bad subscript: %M, for array with dim %M"
           linum tried real
      BadAddress pc targ
        -> printf "on line $d ==> bad target of a GOTO|GOSUB: %d"
           pc targ
      BadType pc expc got
        -> printf "on line %d ==> type mismatch: expected %T, got %T"
           pc expc got
      NextError pc
        -> printf "on line %d ==> NEXT without FOR" pc
      ForError pc
        -> printf "on line %d ==> FOR without NEXT" pc
      RetError pc
        -> printf "on line %d ==> RETURN without having called GOSUB" pc
      WendError pc
        -> printf "on line %d ==> WEND without WHILE" pc
      WhileError pc
        -> printf "on line %d ==> WHILE without WEND" pc

type PC = Int

data FlowCTX
  = InFor
    { lstart  :: PC -- | Where to go each iteration repeat
    , lvar    :: Var -- | Variable we loop over
    , toEnter :: Expr -- | test for entry
    , mstep   :: VMState () -- | What to do each time we come back
    }
  | InWhile
    { lstart :: PC -- | Where to go each iteration repeat
    , toEnter :: Expr -- | test for loop entry, 0 => don't enter
    }
  | InSub
    { lstart :: PC -- | address to return to on a RET
    }

instance Show FlowCTX where
  showsPrec p c =
    case c of
      InFor s v e _ -> showString $
                       "FOR start: " ++ show s ++
                       " var: " ++ show v
      InWhile s _ -> showString $
                     "WHILE start: " ++ show s
      InSub s -> showString $ "SUB start: " ++ show s
                 

  
data EvalState =
  EV { heap :: Map Var Val
     , pc :: Int
     , linumMap :: Map Int Int
     , flowStack :: [FlowCTX]
     , prog :: Vec Stmt
     }

pushFlow :: (PC -> FlowCTX) -> VMState()
pushFlow mkCtx = do
  ev <- get
  let oldStack = flowStack ev
      ctx = mkCtx $ pc ev
  put ev{flowStack = ctx : oldStack}
  logCtx ("new context: " ++ show ctx)
  st <- flowStack <$> get     
  logCtx ("stack is \n" ++ ppShow st)

popFlow :: VMState (Maybe FlowCTX)
popFlow = do
  ev <- get
  case flowStack ev of
    []   -> logCtx "popped from empty stack" >> pure Nothing
    x:xs -> logCtx ("popped " ++ show x) >>
            put ev{flowStack = xs} >>
            pure (Just x)
            
peekFlow :: VMState (Maybe FlowCTX)
peekFlow = do
  mctx <- uncons . flowStack <$> get
  pure $ fst <$> mctx
    
pushRet :: VMState ()
pushRet = pushFlow (InSub . succ)

pushFor :: Var -> Expr -> VMState () -> VMState ()
pushFor var test step = pushFlow $ \pc -> InFor (1 + pc) var test step

pushWhile :: Expr -> VMState()
pushWhile e = pushFlow $ \pc -> InWhile (1 + pc) e

getLineMap :: VMState (Map Int Int)
getLineMap = linumMap <$> get

getPC :: VMState PC
getPC = pc <$> get

getProg :: VMState (Vec Stmt)
getProg = prog <$> get           

setPC :: PC -> VMState ()
setPC newPC = do
  ev <- get
  put ev{pc = newPC}

incrPC :: VMState ()
incrPC = getPC >>= setPC . succ

getRealLine :: VMState Int
getRealLine = (M.!) <$> getLineMap <*> getPC

asIndex :: Val -> VMState Int
asIndex (N d) = pure $ floor d
asIndex s     = badType Numeric $ typeof s

badAddr :: Int -> VMState a
badAddr i = do
  l <- getRealLine
  throwError $ BadAddress l i

badIdx :: Dims -> Dims -> VMState a
badIdx tried real = do
  l <- getRealLine
  throwError $ BadSubscript l tried real

badType :: Type -> Type -> VMState a
badType expect actual = do
  l <- getRealLine
  throwError $ BadType l expect actual

forError, nextError, whileError, wendError :: VMState a
nextError = getRealLine >>= throwError . NextError 
forError = getRealLine >>= throwError . ForError
wendError = getRealLine >>= throwError . WendError
whileError = getRealLine >>= throwError . WhileError
retError = getRealLine >>= throwError . RetError
             

getVar :: Var -> VMState Val
getVar v = accessVar v Nothing

setVar :: Var -> Val -> VMState Val
setVar var = accessVar var . Just
             
accessVar :: Var -> Maybe Val -> VMState Val
accessVar var mval = do
  ev <- get
  let oldHeap = heap ev
  (new, old) <- validate var mval $ M.lookup var oldHeap
  whenJust mval $ const (put ev{heap = M.insert var new oldHeap})
  pure old

-- | Validate does the heavy lifting for variable getting and setting.  It
-- checks index bounds and returns default values for variables that can
-- have them (non-array vars).
validate :: Var -- | variable we're accessing
         -> Maybe Val -- | Maybe we want to change it
         -> Maybe Val -- | What the heap says we have
         -> VMState (Val, Val) -- | (new, old)
validate var mval Nothing =
  case var of
    SArr _ d@(Dims l) -> badIdx d (Dims $ Lit (LNum 0) <$ l)
    NArr _ d@(Dims l) -> badIdx d (Dims $ Lit (LNum 0) <$ l)
    SVar _ -> pure $ (fromMaybe emptyString mval, emptyString)
    NVar _ -> pure $ (fromMaybe zero        mval, zero)
              
validate var mval (Just v@(S _)) =
  pure $ (fromMaybe v mval, v) 
validate var mval (Just v@(N _)) =
  pure $ (fromMaybe v mval, v) 
validate var mval (Just a@(A adim v)) = setAt l a
  where dim@(Dims l) = dimsOf var
        idxErr = badIdx dim adim
        setAt :: [Expr] -> Val -> VMState (Val, Val)
        setAt [] v@(S _) = pure (fromMaybe v mval, v) 
        setAt [] v@(N _) = pure (fromMaybe v mval, v)
        setAt (e:es) (A _ vec) = 
          eval e >>= asIndex >>= \idx ->
          if idx <= V.length vec && idx > 0
          then do
            (new, old) <- setAt es (vec V.! (idx - 1))
            pure (A adim $ V.modify (\v -> V.write v (idx - 1) new) vec, old)
          else idxErr
        setAt _ _ = idxErr

type VMState = ExceptT VMErr (StateT EvalState IO)

logExpr, logLine, logStmt, logCtx, logEnd, logFor, logGoto :: String -> VMState ()
logg :: String -> VMState ()
loggHelp :: Int -> String -> VMState ()        
logExpr = loggHelp 0
logLine = loggHelp 1
logStmt = loggHelp 2
logCtx  = loggHelp 3
logEnd  = loggHelp 4
logFor  = loggHelp 5
logGoto = loggHelp 6          
logg    = loggHelp 100
loggHelp n =
  case n of
--    _ -> const (pure ()) -- All
    0 -> const (pure ()) -- Expr
    1 -> const (pure ()) -- Line
    2 -> const (pure ()) -- Stmt
    3 -> const (pure ()) -- Ctx
    4 -> const (pure ()) -- End
    5 -> const (pure ()) -- For
    6 -> const (pure ()) -- Goto
    _ -> liftIO . putStrLn

  
class Eval a b | a -> b where
  eval :: a -> VMState b

runProg :: Vec Stmt -> Map Int Int -> IO ()
runProg prg lmap =
  let init =  EV { heap = M.empty
                 , pc   = 0
                 , linumMap = lmap
                 , flowStack = []
                 , prog = prg
                 }
  in do
    ret <- runStateT (runExceptT run) init
    case ret of
      (Left err, s) -> putStrLn "ERRORS:" >> print err
      (Right _, s )-> pure ()

          
run :: VMState ()
run = do
  pc <- getPC
  logLine $ "running line " ++ show pc
  prog <- getProg
  case prog !? pc of
    Nothing -> logEnd "Done"
    Just s -> eval s >> run
              
instance Eval Stmt () where
  eval s = logStmt ("eval Statement : " ++ show s) >>
    case s of
      REM t
        -> incrPC
      NOP
        -> incrPC               
      GOTO i
        -> goto i
      GOSUB i
        -> pushRet >> goto i
      ONGOTO e addrs
        -> ongoto e addrs
      ONGOSUB e addrs
        -> pushRet >> ongoto e addrs
      
      LET var e -> eval e >>= setVar var >> incrPC

      -- NEXT without any vars specified implicitly
      NEXT vs -> handleNext vs -- This is a doozy


      FOR var start end mstep
        -> do
        N s <- maybe (pure $ N 1) eval mstep
        N e <- eval end
        let test = if s < 0
                   then Prim (Gt (Var var) (Lit $ LNum e))
                   else Prim (Lte (Var var) (Lit $ LNum e))
        let eachLoop = do
                        res <- eval (Prim (Add (Var var) (Lit $ LNum s)))
                        setVar var res >> pure ()

                             
        eval start >>= setVar var
        N v <- eval test
        if v /= 0
          then pushFor var test eachLoop >> incrPC
          else do
            prog <- getProg
            let isNext (NEXT _) = True
                isNext _        = False
            case V.findIndex isNext prog of
              Nothing -> forError
              Just i  -> setPC i
        

      WHILE mexpr
        -> do
        let test = fromMaybe (Lit $ LNum 1) mexpr
        N enter <- eval test
        if enter /= 0
        then pushWhile test >> incrPC
        else do
          prog <- getProg
          let isWend (WEND _) = True
              isWend _        = False
          case V.findIndex isWend prog of
            Nothing -> whileError
            Just i  -> setPC i

      WEND mexpr
        -> do
        N escape <- eval $ fromMaybe (Lit $ LNum 0) mexpr
        ctx <- popFlow
        case ctx of
          Just (InWhile lstart reenter) ->
            do
              N goback <- eval reenter
              if escape /= 0 || goback == 0
              then incrPC
              else setPC lstart

          _ -> wendError
      IF _ _ _
        -> error "IF statements should have been converted to IFGO by now"

      IFGO e i _
        -> do
        N choice <- eval e
        if choice /= 0
        then goto i
        else incrPC
          
      DIM dims
        -> mapM_ mkdim dims
        where stringName = ("$" `T.isSuffixOf`)
              mkdim (name, d@(Dims es))
                = do
                ev <- get
                sizes <- mapM (eval >=> asIndex) es
                let oldHeap = heap ev
                    (var, baseval)
                      | stringName name = (SArr name d, emptyString)
                      | otherwise = (NArr name d, zero)
                    val = foldr mkArr baseval sizes
                    mkArr size val = A d (V.replicate size val)
                    
                put ev{heap = M.insert var val oldHeap}
                incrPC
                
      RET
        -> whileM $ do
             ctx <- popFlow
             logCtx ("returning, found ctx: " ++ show ctx)
             case ctx of
               Nothing -> retError
               Just (InSub start) -> 
                    setPC start >> pure False
               Just _ -> pure True
      END -> getProg >>= setPC . length -- jump to end
      PRINT [] -> liftIO (putStrLn "") >> incrPC
      PRINT args
        -> do
        foldM printArg 0 args
        case last args of
          PSem _ -> incrPC
          _ -> liftIO (putStrLn "") >> incrPC
        where printArg l a =
                case a of
                  PTab e -> do
                         val <- asIndex =<< eval e
                         if val <= l then pure l
                         else liftIO $ putStr
                                (replicate (val - l) ' ') >> pure val

                  PSem e -> printDelim ""    " " e l
                  PCom e -> printDelim "\t" "\t" e l
                  PReg e -> printDelim ""    " " e l

              printDelim sdel ndel e len =
                do v <- eval e
                   case v of
                     S str ->
                       do liftIO $ T.putStr str
                          liftIO $ putStr sdel
                          pure (len + T.length str)
                     N n ->
                       let str = show n ++ ndel
                       in liftIO $ putStr str >> pure (len + length str)

      INPUT mtext vars
        -> do
        whenJust mtext (liftIO . T.putStr)
        liftIO $ hFlush stdout
        inputs <- loopM takeInput []
        mapM_ (uncurry setVar) $ zip vars inputs
        incrPC
        where
          ty = typeof $ head vars
          filt =
            case ty of
              Stringy -> \line -> pure [S line]
              Numeric ->
                \line ->
                  case parseDoubs line of
                    Just vals -> pure $ map N vals
                    Nothing -> do
                        putStrLn $ "Couldn't parse " ++ show line
                        pure []
                           
          takeInput ls =            
            do
              new <- liftIO (filt =<< T.getLine)
              let acc = ls ++ new
              if length acc >= length vars
              then pure (Right acc)
              else do
                liftIO $ putStr "\n??" >> hFlush stdout
                pure (Left acc)





goto :: PC -> VMState ()                   
goto i =
  logGoto ("GOing to " ++ show i) >>
  ifM
  ((i <) . V.length <$> getProg)
  (setPC i)
  (badAddr i)
                 
ongoto e addrs =
  do
    i <- asIndex =<< eval e
    if i > 0 && i <= length addrs
      then goto (addrs !! (i - 1))
      else incrPC

-- `handleNext`, `matchFors` and `decideFor` manage the logic necessary for
-- determining how to respond to a NEXT statement. The argument lets it
-- determine whether the NEXT was invoked with variables (e.g. `NEXT I, J`) or
-- without. `handlNext` is used with loopM, returning a Right value holding the
-- address to goto once it's determined or a Left value if it needs to keep
-- popping contexts.  There are some opportunities for BASIC runtime errors
-- here: If a NEXT references a variable that isn't an iterator of some loop, or
-- if we are not, in fact, inside a loop.
handleNext [] = loopM matchFors Nothing >>= setPC
handleNext vs = loopM matchFors (Just vs) >>= setPC                
matchFors mvs =
  do
    -- We don't necessarily want to pop a context; that only happens
    -- when the loop terminates. Since that _should_ happen less often,
    -- we'll peek at it first, and actually pop it later if necessary.
    ctx <- peekFlow
    case ctx of
      -- No loop context on the stack: NEXT without FOR error
      Nothing -> nextError
      Just (InFor start lvar test eachLoop)
        -> do
        case mvs of
          -- Implicit NEXT: The InFor we found is the one we use.
          -- Don't give `decide` more variables to test if the loop
          -- terminal condition is met.                    
          Nothing -> logFor "implicit next" >>
                     decideFor test eachLoop [] start
          -- Explicit NEXT                                      
          Just (var:rest)
               -- The InFor is the one we want if its loop variable is
               -- the same as the one at the head of the list.  We
               -- give `decide` the `rest` of the variables, in case
               -- the terminal condition is met and we need to test
               -- another variable.
             | lvar == var ->
               logFor "found the right var" >>
               decideFor test eachLoop rest start
                      
               -- If the variables don't match, we keep looking for
               -- the correct loop context.  This means we don't
               -- change the list of variables NEXT was given.
             | otherwise   ->
               logFor "popping for missed var" >>
               popFlow >> pure (Left $ Just (var:rest))

          -- Only reached when NEXT invoked with explicit variables.
          -- If a variable that isn't an iterator of a FOR loop is
          -- specified, we'll get a "NEXT without FOR" error
          Just [] -> nextError
                     
      -- Context is not a For loop: can't invoke NEXT
      -- when the innermost context is a WHILE or SUB
      Just _  -> nextError

-- We need a lot of context to determine how to proceed once a the
-- right InFor context is found.
decideFor :: Expr -- Expression to test for loop re-entry
          -> VMState () -- VMState update 
          -> [Var] -> PC
       -> VMState (Either (Maybe [Var]) PC)
decideFor test loop more start =
  do
    -- `loop` is the state-changing update that increments
    -- or decrements the loop variable.
    loop
    N enter <- eval test
    logFor $ "test is " ++ show test
    logFor $ "result is " ++ show enter
    pc <- getPC
    -- If `enter` is True, we jump back to the loop start
    if | enter /= 0 -> pure (Right start)
       -- Else, if there are no more variables to test, we simply move
       -- on to the next statement
       | null more  -> popFlow >> pure (Right $ pc + 1)
       -- If there _are_ variables remaining, we need to keep popping
       -- contexts to determine where to go
       | otherwise  -> popFlow >> pure (Left $ Just more)


instance Eval Var Val where
  eval = getVar -- much nicer than Stmt
         
instance Eval Expr Val where
  eval e = do
    logExpr ("evaling " ++ show e)
    res <- case e of
             Lit l   -> eval l
             Prim op -> eval op
             Paren e -> eval e
             Var v   -> eval v

    logExpr ("got " ++ show res)
    pure res
      
instance Eval (Op Expr) Val where
  eval o =
    case o of
      Rnd a -> do
        n <- eval a
        case n of
          N d | d < 0 -> error "RND seeding not implemented yet"
              | d == 1 -> liftIO randomIO >>= pure . N
              | otherwise -> liftIO (randomRIO (0, d)) >>= pure . N

      Chp a -> do
             N d <- eval a
             pure (N $ fromIntegral $ floor d)

      Neg a -> do
             N d <- eval a
             pure . N  $ negate d
      Not a -> do
             N d <- eval a
             pure $ if d == 0
                    then N 1
                    else N 0
      Add a b ->
        case typeof a of
          Stringy -> do
              S l <- eval a
              S r <- eval b
              pure $ S (l <> r)
          Numeric -> numericOp a b (+)
      Sub a b -> numericOp a b (-)
      Div a b -> numericOp a b (/)
      Mul a b -> numericOp a b (*)
      Pow a b -> numericOp a b (**)
      Mod a b -> numericOp a b (%)
      And a b -> numericOp a b (.&.)
      Or  a b -> numericOp a b (.|.)
      Xor a b -> numericOp a b xor
      Eq  a b -> overloadedOp a b (fromBool (==))
      Neq a b -> overloadedOp a b (fromBool (/=))
      Gt  a b -> overloadedOp a b (fromBool (>))
      Gte a b -> overloadedOp a b (fromBool (>=))
      Lt  a b -> overloadedOp a b (fromBool (<))
      Lte a b -> overloadedOp a b (fromBool (<=))

-- | Turn a comparison operator into a C-like operator, returning 1 for True
-- and 0 for False.
fromBool :: Ord a => (a -> a -> Bool) -> (a -> a -> Doub)
fromBool op a b | a `op` b  = 1
                | otherwise = 0
numericOp a b op = do
  N l <- eval a
  N r <- eval b
  pure $ N (l `op` r)

-- Helper for evaluating operators that work on Numerics and Strings
overloadedOp :: Expr -> Expr
             -- Holy crap! A practical use of RankNTypes
             -> (forall a . Ord a => a -> a -> Doub)
             -> VMState Val
overloadedOp a b op = do
  l <- eval a
  r <- eval b
  case (l, r) of
    -- This is why we need RankNTypes.  The `op` is valid for any type installed
    -- in Ord.  Without higher rank types, trying to use it here will fail
    -- unification because it could only be inferred as an operator on Text or Doub
    (S x, S y) -> pure $ N (x `op` y)
    (N x, N y) -> pure $ N (x `op` y)
    (S _, _  ) -> badType Stringy Numeric
    _ -> badType Numeric Stringy

instance Eval Literal Val where
  eval (LNum d) = pure $ N d
  eval (LStr d) = pure $ S d
