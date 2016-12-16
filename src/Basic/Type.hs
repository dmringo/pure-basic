{-# LANGUAGE FlexibleInstances, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Basic.Type
  ( typecheck
  , TypeException (..)
  )
  where

import Basic.AST

import Data.Typeable
import Control.Exception  
import Data.Monoid hiding (Any, All)
import Data.Text hiding (map, tail, zipWith, foldr)
import qualified Data.Text as T
import Prelude hiding (unlines, null)  

            
typecheck :: Program -> Either SomeException Program
typecheck p = case unifies p of
                   Yes t  -> Right p
                   x -> Left . toException . TypeException $ show x
                
text :: Show a => a -> Text
text = pack . show        
  
data Type = Numeric | Stringy | Void | Any | All | None
            deriving (Eq, Show)


data TCResult = No [(Text, Type, Type)] | Yes Type

instance Show TCResult where
  showsPrec p (No whys) =
    let mkMsg (a,b,c) = unlines
                        [ a
                        , "Expected " <> text b
                        , "Got " <> text c]
    in showString . unpack . mconcat $ map mkMsg whys
  showsPrec p (Yes t) = showString $ "Yes " ++ show t
                        

newtype TypeException = TypeException String
  deriving (Typeable)

instance Show TypeException where
  showsPrec p (TypeException msg) =
    showString msg

instance Exception TypeException 

instance Monoid TCResult where
  mempty = Yes Any
  No a    `mappend` No b  = No $ a <> b
  No a  `mappend` _       = No a
  _       `mappend` No a  = No a
  Yes Any `mappend` Yes t   = Yes t
  Yes t   `mappend` Yes Any = Yes t
  Yes All `mappend`  _      = Yes All  
  _       `mappend` Yes All = Yes All
  Yes t1  `mappend` Yes t2
    | t1 /= t2   ||
      t1 == None ||
      t2 == None  = No [(mempty, t1, t2)]
    | otherwise = Yes $ typeof t1

class TC a where
  unifies :: a -> TCResult
  -- Probably don't need this
  typeof  :: a -> Type

infixl 7 ~=
(~=) :: (TC a, TC b, Show b) => a -> b -> TCResult        
a ~= b = case unifies a of
           y@(Yes _) -> b ?> y <> unifies b
           no        -> no

infixr 0 ?>         
(?>) :: Show a => a -> TCResult -> TCResult
a ?> No ((msg, t1,t2):ts) = No $ (unlines ["In " <> text a, msg], t1, t2):ts
a ?> r      = r
                     

-- This is a bit of a hack but it makes typechecking a little more convenient in
-- a few places.
instance TC Type where
  unifies = Yes
  typeof  = id

instance TC TCResult where
  unifies = id
  typeof (Yes t) = t
  typeof (No _)  = Void -- Probably bad
               
instance TC Expr where
  unifies e =
    case e of
      FunCall (toLower -> name) args
        | isNumFun name -> mconcat . map (Numeric ~=) $ args
        | otherwise -> error ("unknown function in TC? " ++ show name)

      Paren e -> unifies e
      Prim op -> unifies op
      Var v   -> unifies v
      Lit l   -> unifies l

    where 

  typeof e =
    case e of
      FunCall (toLower -> name) args
        | isNumFun name -> Numeric
        | otherwise -> error ("unknown function in TC? " ++ show name)

      Paren e -> typeof e
      Prim op -> typeof op
      Var v   -> typeof v
      Lit l   -> typeof l

-- Check the two "builtin" functions
isNumFun :: Text -> Bool               
isNumFun n = n == "rnd" || n == "int"
             

instance TC (Op Expr) where
  -- Ops always return numeric types
  typeof op = Numeric
              
  unifies op = 
    case op of
      Neg arg -> Numeric ~= arg

      Not arg -> Numeric ~= arg

      -- Overloaded operators
      Add a1 a2 -> a1 ~= a2
      Eq  a1 a2 -> a1 ~= a2
      Neq a1 a2 -> a1 ~= a2
      Gt  a1 a2 -> a1 ~= a2
      Lt  a1 a2 -> a1 ~= a2
      Gte a1 a2 -> a1 ~= a2
      Lte a1 a2 -> a1 ~= a2

      -- Strictly numeric
      Sub a1 a2 -> Numeric ~= a1 ~= a2
      Div a1 a2 -> Numeric ~= a1 ~= a2
      Mul a1 a2 -> Numeric ~= a1 ~= a2
      Pow a1 a2 -> Numeric ~= a1 ~= a2
      Mod a1 a2 -> Numeric ~= a1 ~= a2
      And a1 a2 -> Numeric ~= a1 ~= a2
      Or  a1 a2 -> Numeric ~= a1 ~= a2
      Xor a1 a2 -> Numeric ~= a1 ~= a2


instance TC Dimension where
  typeof = const Void
  unifies d =
    case d of
      (D1 e)     -> Numeric ~= e
      (D2 e1 e2) -> Numeric ~= e1 ~= e2

instance TC PrintArg where
  typeof = const Any -- Maybe should inherit inner expr type?
  unifies a =
    case a of
      (PTab e) -> Numeric ~= e ~= All
      (PSem e) -> e ~= All
      (PCom e) -> e ~= All
      (PReg e) -> e ~= All
         
instance TC Stmt where
  typeof = const Void
  unifies s =  s ?>
    case s of
      REM _       -> mempty
      GOTO _      -> mempty
      GOSUB _     -> mempty
      RET         -> mempty
      END         -> mempty
      NOP         -> mempty
      PRINT args  -> unifies args
      INPUT _ vs  -> foldr (~=) mempty vs
      ONGOTO e _  -> Numeric ~= e
      ONGOSUB e _ -> Numeric ~= e
      NEXT vars   -> foldr (~=) (Yes Numeric) vars
      WHILE mexpr -> Numeric ~= mexpr
      WEND mexpr  -> Numeric ~= mexpr
      LET v u     -> v ~= u
      IFGO e _    -> Numeric ~= e
      DIM dims    -> mconcat . map (unifies . snd) $ dims
      FOR i init end step ->
        Numeric ~= i ~= init ~= end ~= step
      IF p ts es  -> p ~= ts ~= es

instance TC Line where
  typeof  = const Void
  unifies l = l ?> unifies . snd $ l
                         
instance TC Var where
  unifies           = Yes . typeof
  typeof (SVar _  ) = Stringy
  typeof (SArr _ _) = Stringy
  typeof (NVar _  ) = Numeric
  typeof (NArr _ _) = Numeric

instance TC Literal where
  unifies         = Yes . typeof
  typeof (LNum _) = Numeric
  typeof (LStr _) = Stringy
            
instance TC a => TC [a] where
  unifies as = mconcat $ map unifies as
  typeof     = typeof . unifies

instance TC a => TC (Maybe a) where
  unifies         = Yes . typeof
  typeof (Just a) = typeof a
  typeof Nothing  = Any                     
  
