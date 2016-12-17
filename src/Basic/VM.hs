

module VM where


import Basic.AST as AST
import Basic.Doub
import Data.Text  

-- In a basic program, need to keep track of the loop context.  In general,
-- WHILE and WEND statements do not need to be matched in the text of the
-- program.  A WHILE loop test (at the WHILE) that fails, causing loop exit,
-- will jump to the statement directly following the last WEND that was reached.
-- If there is no WEND "numerically" following the WHILE and the WHILE loop is
-- not entered (terminal condition is false, initially), we get an error.  This
-- means we need to have an inital target for loop exit that may be overwritten
-- at any time.  Further, since loops can be nested, we need some notion of a
-- stack to keep track of the previous loop exit values.
--
-- FOR loop context must be maintained in the same way, with the additional
-- requirement that the indexing variable(s) are also recorded in a stack-like
-- structure, since a NEXT statement without an explicit variable increments the
-- variable in the innermost FOR loop context.  These contexts are not
-- decideable at "compile" time, so must be encoded in the runtime.
--
-- The problem of deciding where a loop exits has a parallel in the problem of
-- determining where a NEXT or WEND directs control flow for the same reason.
-- 

-- | an address in the program's instructions
type PAddr = Int


data MAddr
  = Simple Int
  | Indirect ( Int -- Addr in Heap
             , Int -- Row index (invariant, 1 for 1D arrays)
             , Int -- Col index
             ) 
    deriving (Show)

-- | VM-level values
data Value
  = S Text
  | D Doub
    
  -- This is simpler than indexing off a base address for arrays and worrying
  -- about dynamic redimensioning clobbering other addresses.  Thank goodness
  -- for managed memory.
  --
  -- Int pairs are the nominal dimensions of the array, row major.  1D arrays
  -- have dimensions (1, N).  These dimensions are changed by the ALOC
  -- instruction, and are used for calculating offsets into the vector.
  | VS (Int, Int) (Vec Text) -- | Array of Text
  | VD (Int, Int) (Vec Doub) -- | Array of Numerics
  deriving (Show)

type LoopContext =
  ( PAddr -- | Entrance
  , PAddr -- | Exit
  , Maybe Int
  -- | Address of loop variable, if present.
  -- | Since FOR loop vars are always simple, we just use an Int here
    
  , Doub  -- | Step size for a loop variable
  )

  
data VMState = VM
  { loopStack :: [LoopContext] -- | Stack of loop contexts
  , dataStack :: [Value] -- | Expression eval stack
  , heap      :: (Vec Value) -- | for named variables
  }


data Target
  = Const Value
  | InHeap MAddr
  | Stack
  | LoopStack deriving (Show)

data Instr
  = OPR (AST.Op Target)
  -- | perform Op, store result in:
  --   * first (and only) argument for unary ops (if non-Const)
  --   * second argument for binary ops (if non-Const)
  --  If target for store is a Const, result is stored at top of dataStack
    
  | JMP PAddr
  -- | Jump to addr
    
  | JNZ PAddr
  -- | Jump to addr if last result (top of dataStack) is non-zero
    
  | OUT (PrintArg Target)
  -- | Print a Target to stdout
    
  | IN  Target
  -- | read input from stdin (prompt is handled by OUT Instr)
    
  | MOV Target Target
  -- | Move value from first arg to second

  | RND Target
  -- | Builtin Random function, for simplicity

  | INT Target
  -- | Builtin Integer truncation (towards -INF). x can (proably) be up to
  -- 2^53-1 and still get the correct result, if I remember how floating point
  -- values work.

  | ALOC -- | (Re)Allocate space for an array.  If dimension values are smaller
         -- than dimensions of array, nothing happens.  If larger, new space is
         -- set to either 0 or the empty string.  Addresses in array will likely
         -- not be consistent after redimensioning (as in Chipmunk)
    Int -- | Heap Address
    Target -- | First dimension (1 generated for 1D DIM statements)
    Target -- | Second dimension
    

    
    

