{-# LANGUAGE NoImplicitPrelude, PatternSynonyms, ViewPatterns #-}

module Basic.AST
  ( Loc
  , Stmt (..)
  , Program
  )
where


import Basic.Prelude
import Data.Vector

type Vec = Vector
type Program = Vec Line
type Line = Vec Stmt
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


data Name = Name Text Uniq
data Var
  = SVar Name -- | String variable
  | NVar Name -- | Numeric variable

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
    Var  -- | Loop variable
    Expr -- | Initial value of loop var.
    Expr -- | Limit of loop var (inclusive)
    Expr -- | How much to change loop var by (calculated once, at beginning)

  | NEXT -- | Increment a FOR loop variable
                                    
  | WHILE -- | General loop until terminal condition
    (Maybe Expr)  -- | Beginng test: Enter loop body if non-zero

  | WEND  -- | End of a WHILE loop
    (Maybe Expr)  -- | End test: Return to loop beginning only if zero
                                          
  | IF   -- | Conditional branching (single line form)
    Expr -- | Predicate test.
    [Stmt] -- | Consequents
    [Stmt] -- | Alternatives
      
  | IFML -- | Conditional branching across multiple lines
    Expr -- | Predicate test
    Addr -- | Where to jump if predicate is false (either the ELSE block or
         --  after an ENDIF)
    
  | DIM -- | Array declaration (and dimensioning)
    [(Var, Dimension)] -- | List of array descriptions
      
  | RET -- | Return statement.  Signifies a GOTO the last GOSUB or ONGOSUB
        -- statement.
                                                                  
  | END -- | Terminates the program

  | PRINT -- | Print statement
    [PrintArg] -- | List of print args

  | INPUT
    Text  -- | Prompt
    [Var] -- | Variables to bind

data PrintArg
  = Tab Expr -- | Move print cursor to a given horizontal positition this can
             -- not move it backwards (according to ref impl)
    
  | Com Expr -- | Print an expression followed by a tab/newline
             -- depending on whether the Expr is the last in the arg list
      
  | Sem Expr -- | Print an expression with tab replaced by a space or with
             -- newline suppressed

    
data Dimension
  = D1 Expr
  | D2 Expr Expr

data Expr
  = Prim (Op Expr)
  | Paren Expr
  | FunCall
    Var -- | Function name
    (Vec Expr) -- | Parameters
  | Index
    Var -- | Array being indexed into
    (Vec Expr) -- |

  | Var Var

  | Lit Literal

data Literal
  = LInt Int16
  | LFlt Float
  | LStr Text
  | LBool Bool
    
data Op argType
  = Neg argType
  | Add argType argType
  | Sub argType argType
  | Div argType argType
  | Mul argType argType
  | Pow argType argType
  | Mod argType argType
  | And argType argType
  | Or  argType argType
  | Not argType argType
  | Xor argType argType
  | Eq  argType argType
  | Neq argType argType
  | Gt  argType argType
  | Gte argType argType
  | Lt  argType argType
  | Lte argType argType

  

pattern Nil :: Vec a
pattern Nil <- (null -> True)
  where Nil = empty -- Now it's bidirectional
pattern V x xs <- (tupd head init -> (x,xs))
  where V = cons

tupd f g x = (f x, g x)

foo           :: Vec a -> Maybe a
foo Nil       = Nothing
foo (x `V` _) = Just x



