

module Basic.CodeGen
  ( remap
  , linearize
  )
  where

import Basic.AST

import Data.Map (Map)
import Data.Monoid  
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.State 
import Data.List (sortOn)


       
-- | Turn a program into a Vector of statements with addresses resolved to
-- vector indicies.
remap :: Program -> (Vec Stmt, Map Int Int)
remap lines = (V.map applyMap stmts, lmap)
  where (stmts, mapping, lmap) = linearize lines
        lookupOrElse i =
          maybe (error $ "missing address in linearization: " ++ show i) id .
          M.lookup i
        resolve i = lookupOrElse i mapping
        applyMap s =
          case s of
            GOTO i       -> GOTO      $ resolve i
            GOSUB i      -> GOSUB     $ resolve i
            ONGOTO e is  -> ONGOTO  e $ map resolve is
            ONGOSUB e is -> ONGOSUB e $ map resolve is
            IFGO e i generated
              | generated -> IFGO e i generated
              | otherwise -> IFGO e (resolve i) generated
            _            -> s
                        

-- | Turn a program into a Vector of statments, providing a Map to indicate at
-- what index in the Vector the statement starting some Line can be found.
linearize
  :: Program
  -> ( Vec Stmt
     -- | Program now a sequence of statments in the ordering implied
     -- by line numbering in the source program
     , Map LineNum Int
     -- | Mapping from line number targets in the source program to Vector
     -- indicies.

     , Map Int LineNum
     -- | Mapping from statement indices to LineNum they came from
     )

linearize lines = (stmts, targMap, stmtMap)
  where
    ordered = sortOn fst lines
    (stmts, targMap, stmtMap) =
      foldl combine (V.empty, M.empty, M.empty) ordered
    combine (vec, targMap, stmtMap) (linum, stmts) =
      let current = V.length vec
          realStmts = unfoldIFs current stmts
      in
        ( vec <> V.fromList realStmts -- Add the new statements
        
        -- Current vector length indicates where (i.e. Vec index) in the program
        -- this line's LineNum is now mapped to.
        , M.insert linum current targMap

        -- Want to associate this linum with everything from current to (length
        -- stmts - 1)
        , M.union stmtMap . M.fromList .
          zip [current .. current - 1 + length realStmts] $ repeat linum
        )

-- We want to take lines/statements of the form:
--   `10 IF exp THEN A : B : C ELSE D : E : F`
--   `11 X`
-- and turn it into
--   `10 IF exp THEN 15`
--   `11 A`
--   `12 B`
--   `13 C`
--   `14 GOTO 18`
--   `15 E`
--   `16 F`
--   `17 G`
--   `18 X`


unfoldIFs :: Int -> [Stmt] -> [Stmt]
unfoldIFs addr = concatMap expand
  where expand (IF e consqs alts) =
          let
            expandedAs = maybe [] (unfoldIFs addr) alts
            cLen       = length expandedCs
            aLen       = length expandedAs + 1 -- for the final GOTO at end
            expandedCs = unfoldIFs (addr + aLen) consqs
            jmpCons    = IFGO (Lit $ LNum 1) (addr + aLen + cLen + 1) True
          in IFGO e (addr + aLen + 1) True : expandedAs ++ jmpCons : expandedCs
        expand s = [s]
          
          
