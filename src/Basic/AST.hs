{-# LANGUAGE PatternSynonyms, ViewPatterns,
TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Basic.AST
  ( LineNum, Program, Vec, Addr, Name, Line
  , Var(..), nameOf, dimsOf
  , Stmt(..),  PrintArg(..), Expr(..), Literal(..)
  , Dims(..), Op(..)
  )
  where



import Basic.Doub

import Data.Ord (comparing)
import Data.Function (on)  
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T

import Text.Printf
import Basic.Unparse  

  
type LineNum = Int  
type Vec = V.Vector
type Program =  [Line]
type Line = (LineNum, [Stmt])
type Uniq = Int


type Addr =
  ( Int
  -- ^ Declared line number e.g. 10 PRINT "HELLO "
  , Int
  -- ^ Statement index on that line.
  -- This is useful when dealing with loop syntax and structure
  -- e.g.
  --
  -- 10 FOR I = 1 TO 10: FOR J = 1 TO I
  -- 20 ...
  -- 30 NEXT I, J : REM equivalent to NEXT I: NEXT J
  --
  -- The NEXT statement is effectively a GOTO, but we want
  -- finer granulariy than the simple line numbers as targets.
  -- Instead of thinking of these as GOTOs, we could maintain
  -- the body of a loop as part of a data structure, but that limits
  -- the ability to GOTO a line inside the loop.
  -- Thinking of the program as a 2D array of statements should permit
  -- a simple interpreter model (I think).
  )


type Name = Text

newtype Dims = Dims [Expr] deriving (Show)

instance PrintfArg Dims where
  formatArg d fmt
    | fmtChar (vFmt 'M' fmt) == 'M'
    = formatString (pretty d) fmt{ fmtChar = 's'
                                 , fmtPrecision = Nothing }
    | otherwise
    = errorBadFormat $ fmtChar fmt
  
data Var
  = SVar Name -- | String variable
  | NVar Name -- | Numeric variable
  | SArr Name Dims -- | Indexed numeric array variable
  | NArr Name Dims -- | Indexed numeric string variable
  deriving Show

nameOf :: Var -> Name
nameOf (SVar n) = n
nameOf (NVar n) = n
nameOf (SArr n _) = n
nameOf (NArr n _) = n

dimsOf (SArr _ d) = d
dimsOf (NArr _ d) = d
                    
instance Eq Var where
  a == b = a `compare` b == EQ
                    
instance Ord Var where
  SVar a   `compare` SVar b   = a `compare` b
  NVar a   `compare` NVar b   = a `compare` b
  SArr a _ `compare` SArr b _ = a `compare` b
  NArr a _ `compare` NArr b _ = a `compare` b
  SArr a _ `compare` _        = GT
  _        `compare` SArr _ _ = LT
  NArr _ _ `compare` _        = GT
  _        `compare` NArr _ _ = LT
  SVar   _ `compare` _        = GT
  _        `compare` SVar _   = LT                                
                                



data Stmt
  = REM Text -- | A Comment
      
  | GOTO -- | GOTO statement
    Int -- | Line number target
      
  | ONGOTO -- | _Indexed_ GOTO, Expr indexes into a list of target line numbers
    Expr -- | Expression whose integer value selects a line number target.
         -- Values greater than the number of targets let control fall through.
    [Int] -- | List of line number targets
            
  | GOSUB -- | GOTO with return
    Int -- | Line number target
                  
  | ONGOSUB -- | _Indexed_ GOSUB, Expr indexes into a list of target line
            -- numbers
    Expr -- | Expression whose integer value selects a line number target.
         -- Values greater than the number of targets let control fall through.
    [Int] -- | List of line number targets
      
  | LET  -- | Variable assignment
    Var  -- | Variable assigned to
    Expr -- | Value assigned
                              
  | FOR  -- | Ranged looping
    Var  -- | Loop variable. Invariant: Var is always a NVar
    Expr -- | Initial value of loop var.
    Expr -- | Limit of loop var (inclusive)
    (Maybe Expr) -- | How much to change loop var by (calculated once, at beginning).
                 -- When Nothing, step is 1

  | NEXT -- | Increment a FOR loop variable
    [Var] -- | The variable(s) to increment.
          -- If Nothing, increment "innermost" FOR loop variable
                                    
  | WHILE -- | General loop until terminal condition
    (Maybe Expr)  -- | Beginng test: Enter loop body if non-zero

  | WEND  -- | End of a WHILE loop
    (Maybe Expr)  -- | End test: Return to loop beginning only if zero

{- 
Supporting IF blocks in *this* way is a bit tricky.
They could be supported in the interpreter if operated on the full
syntax, albeit somewhat awkwardly, as a special case of a normal
IF statement -- alone on a line with no consequents or alternatives.

Getting it right without incurring a lot of redundant parsing would require the
Line parser to have an alternative for the if-block syntax. This constructor
would have to hold lines of the program, making the transformation to the
runtime form probably a bit harder.
  | IFML -- | If conditional branching across multiple lines
    Expr -- | Predicate test
    Addr -- | Where to jump if predicate is false (either the ELSE block or
         --  after an ENDIF)

-}
  | IF   -- | Conditional branching (single line form)
    Expr -- | Predicate test.
    [Stmt] -- | Consequents
    (Maybe [Stmt]) -- | Alternatives
      

  | IFGO -- | Conditional jump to line
    Expr -- | Predicate test.
    Int -- | Where to jump
    Bool -- | HACK: Did I generate this or was it present in the source?

    
  | DIM -- | Array declaration (and dimensioning)
    [(Name, Dims)] -- | List of array descriptions
      
  | RET -- | Return statement.  Signifies a GOTO the last GOSUB or ONGOSUB
        -- statement.
                                                                  
  | END -- | Terminates the program

  | PRINT -- | Print statement
    [PrintArg Expr] -- | List of print args

  | INPUT
    (Maybe Text)  -- | Prompt
    [Var] -- | Variables to bind

  | NOP -- | Do-nothing statement
        -- `10 PRINT "HI" : : : : : rem this is completely legal BASIC`
    deriving (Show)

    
data PrintArg argType
  = PTab argType -- | Move print cursor to a given horizontal positition this can
             -- not move it backwards (according to ref impl)
    
  | PCom argType -- | Print an expression followed by a tab
      
  | PSem argType -- | Print an expression followed by:
             --  * a space, if the expression is numeric
             --  * nothing, if the expression is a string
             -- `10 PRINT "there "; "are"; 2; "ducks";`
             -- `there are 2 ducks >`
  | PReg argType -- | Print an expression identically to PSem, unless it
              -- is the last element in the list of PrintArgs
    deriving (Show)

    
data Expr
  = Prim -- | A "primitive" or builtin operation.
    (Op Expr) -- | The actual operator invoked

  | Paren -- | Expression grouped for precedence
    Expr -- | Inner expressions

  {- It was a little silly to implement the INT and RND builtins like this.
     Function calls might be implemented someday, but for now, they're not

  | FunCall -- | Function call.
    Name -- | Function name
    [Expr] -- | Parameters
   -}
  | Var -- | A variable.
    Var -- | The variable ¯\_(ツ)_/¯

  | Lit -- | A literal value 
    Literal -- | The underlying literal
    deriving (Show)



          
data Literal
  = LNum Doub -- | See Basic.Doub
  | LStr Text
    deriving (Show)

    
data Op argType
  = Neg argType -- | Arithmetic negation `-5 -> 5`
  | Not argType -- | Logical not: `NOT 1.2 -> 0`, `NOT 0 -> 1`
  | Chp argType -- | Integer truncation: `INT(3.6) -> 3`
  | Rnd argType -- | Return a random number. See basic.manual.txt
  | Add argType argType -- | Addition OR string concatenation.
  | Sub argType argType -- | Subtraction
  | Div argType argType -- | Division
  | Mul argType argType -- | Multiplication
  | Pow argType argType -- | Exponentiation
  | Mod argType argType -- | Modulus operator
  | And argType argType -- | Bitwise AND 
  | Or  argType argType -- | Bitwise OR
  | Xor argType argType -- | Bitwise XOR
  | Eq  argType argType -- | Comparing Numbers and Strings
  | Neq argType argType 
  | Gt  argType argType
  | Gte argType argType
  | Lt  argType argType
  | Lte argType argType
    deriving (Show)

  
-- Unparsing

statements :: [Stmt] -> Doc
statements =  hsep . punctuate colon . map unp

instance Unparse Dims where
  unp (Dims l) =
    parens . hsep . punctuate comma $ map unp l
           

instance Unparse Line where
  unp (n, ss) =
    int n <+> statements ss

instance Unparse Literal where
  unp (LStr t) = quotes $ unp t
  unp (LNum d) = unp d

instance Unparse a => Unparse (PrintArg a) where
  unp a =
    case a of
      PTab v -> "TAB" <> parens (unp v)
      PCom v -> unp v <> comma
      PSem v -> unp v <> semi
      PReg v -> unp v

instance Unparse Expr where
  unp e =
    case e of
      Prim op -> unp op
      Paren e -> parens $ unp e
      Var v -> unp v
      Lit l -> unp l

instance Unparse Var where
  unp v =
    case v of
      SVar n -> unp n
      NVar n -> unp n
      SArr n d -> unp n <> unp d
      NArr n d -> unp n <> unp d

instance Unparse a => Unparse (Op a) where 
  unp o =
    case o of
      Neg a -> minus <> unp a
      Not a -> "NOT" <+> unp a
      Chp a -> "INT" <> parens (unp a)
      Rnd a -> "RND" <> parens (unp a)
      Add a b -> mk a plus   b
      Sub a b -> mk a minus  b
      Div a b -> mk a divide b
      Mul a b -> mk a times  b
      Pow a b -> mk a tothe  b
      Mod a b -> mk a "MOD"  b
      And a b -> mk a "AND"  b
      Or  a b -> mk a "OR"   b
      Xor a b -> mk a "XOR"  b
      Eq  a b -> mk a equals b
      Neq a b -> mk a "<>"   b
      Gt  a b -> mk a ">"    b
      Gte a b -> mk a ">="   b
      Lt  a b -> mk a "<"    b
      Lte a b -> mk a "<="   b
    where mk a op b = unp a <+> op <+> unp b
        
instance Unparse Stmt where
  unp s =
    case s of
      REM t
        -> "REM" <+> unp t
      
      GOTO i
        -> "GOTO" <+> unp i
      
      ONGOTO e is
        -> "ON" <+> unp e <+> "GOTO" <+> hsep (map unp is)
            
      GOSUB i
        -> "GOSUB" <+> unp i

      ONGOSUB e is
        -> "ON" <+> unp e <+> "GOSUB" <+> hsep (map unp is)
      
      LET v e
        -> "LET" <+> unp v <+> equals <+> unp e
                              
      FOR v b e ms
        -> "FOR" <+> unp v <+> equals <+> unp b <+>
           "TO"  <+> unp e <+>
           maybe empty (("STEP" <+>) . unp) ms
           
      NEXT vs
        -> "NEXT" <+> hsep (punctuate comma $ map unp vs)
                                    
      WHILE me
        -> "WHILE" <+> unp me

      WEND  me
        -> "WEND" <+> unp me

      IF e cs mas
        -> "IF" <+> unp e <+> "THEN" <+> statements cs <+>
           maybe empty statements mas
                 
      IFGO e i _
        -> "IF" <+> unp e <+> "THEN" <+> int i

      DIM assoc
        -> "DIM" <+> hsep (punctuate comma dims)
        where dims = [text (T.unpack n) <+> unp d | (n,d) <- assoc]
          
      RET -> "RET"                                                                  
      END -> "END"

      PRINT args
        -> "PRINT" <+> hsep (map unp args)

      INPUT prompt vars
        -> "INPUT" <+> maybe empty (quotes . text . T.unpack) prompt <+>
           hsep (punctuate comma $ map unp vars)

      NOP -> " "


instance Unparse (Vec Stmt) where
  unp v = vcat . V.toList $ V.imap
          (\i s -> unp i <+> unp s) v
             
