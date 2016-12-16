{-# LANGUAGE OverloadedStrings #-}


module Basic.Validate where

import Basic.AST
import Basic.Type
import Basic.Parser
  
import Control.Exception
import Control.Monad  
import Data.Text hiding (foldr)
import qualified Data.Set as Set
import Data.Monoid  
import Data.Typeable  
import Text.Show.Pretty

type Validation = Program -> Either SomeException Program

newtype BadLineNumException = BadLineNumException [Int]

instance Exception BadLineNumException
instance Show BadLineNumException where
  showsPrec p (BadLineNumException i) =
    showString $ "Duplicate line numbering(s): " ++ show i

badLines :: [Int] -> SomeException
badLines = SomeException . BadLineNumException

validate :: Validation
validate = checkLineNumbers >=> typecheck

checkLineNumbers :: Validation
checkLineNumbers lines
  | (dups, _) <- foldr findDup (Set.empty, Set.empty) lines
  , not $ Set.null dups = Left . badLines $ Set.toList dups
  | otherwise = Right lines
  where findDup (num, _) (dups, orig)
          | num `Set.member` orig = (Set.insert num dups, orig)
          | otherwise = (dups, Set.insert num orig)
