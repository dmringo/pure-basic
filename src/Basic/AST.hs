{-# LANGUAGE
NoImplicitPrelude, PatternSynonyms, ViewPatterns,
StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Basic.AST where


import Basic.Prelude
import Basic.Doub

import Data.Vector
import Data.Text (Text)
import qualified Data.Text as T

type Vec = Vector
type Program =  [Line]
type Line = (Int, [Stmt])
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
data Var
  = SVar Name -- | String variable
  | NVar Name -- | Numeric variable
  | SArr Name Dimension -- | Indexed numeric array variable
  | NArr Name Dimension -- | Indexed numeric string variable
  deriving Show

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
    
  | DIM -- | Array declaration (and dimensioning)
    [(Name, Dimension)] -- | List of array descriptions
      
  | RET -- | Return statement.  Signifies a GOTO the last GOSUB or ONGOSUB
        -- statement.
                                                                  
  | END -- | Terminates the program

  | PRINT -- | Print statement
    [PrintArg] -- | List of print args

  | INPUT
    (Maybe Text)  -- | Prompt
    [Var] -- | Variables to bind

  | NOP -- | Do-nothing statement
        -- `10 PRINT "HI" : : : : : rem this is completely legal BASIC`
    deriving (Show)

    
data PrintArg
  = PTab Expr -- | Move print cursor to a given horizontal positition this can
             -- not move it backwards (according to ref impl)
    
  | PCom Expr -- | Print an expression followed by a tab
      
  | PSem Expr -- | Print an expression followed by:
             --  * a space, if the expression is numeric
             --  * nothing, if the expression is a string
             -- `10 PRINT "there "; "are"; 2; "ducks";`
             -- `there are 2 ducks >`
  | PReg Expr -- | Print an expression identically to PSem, unless it
              -- is the last element in the list of PrintArgs
    deriving (Show)

    
data Dimension
  = D1 Expr
  | D2 Expr Expr
    deriving (Show)

data Expr
  = Prim -- | A "primitive" or builtin operation.
    (Op Expr) -- | The actual operator invoked

  | Paren -- | Expression grouped for precedence
    Expr -- | Inner expressions
    
  | FunCall -- | Function call.
            -- May be useful for user defined functions/subroutines, if added,
            -- but now only used for builtin functions (INT, ABS, etc)
    Name -- | Function name
    [Expr] -- | Parameters

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
  = Neg argType
  | Not argType 
  | Add argType argType
  | Sub argType argType
  | Div argType argType
  | Mul argType argType
  | Pow argType argType
  | Mod argType argType
  | And argType argType
  | Or  argType argType
  | Xor argType argType
  | Eq  argType argType
  | Neq argType argType
  | Gt  argType argType
  | Gte argType argType
  | Lt  argType argType
  | Lte argType argType
    deriving (Show)

  

pattern Nil :: Vec a
pattern Nil <- (null -> True)
  where Nil = empty -- Now it's bidirectional
pattern V x xs <- (tupd head init -> (x,xs))
  where V = cons

tupd f g x = (f x, g x)

foo           :: Vec a -> Maybe a
foo Nil       = Nothing
foo (x `V` _) = Just x



