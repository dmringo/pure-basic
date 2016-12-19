module Main where

import Basic.Parser
import Basic.Type
import Basic.Validate
import Basic.CodeGen
import Basic.Unparse
import Basic.Eval  
import Basic.Doub
  
import System.Environment
import Control.Monad  
  
main :: IO ()
main = do
  name:_ <- getArgs
  runit name

runit :: String -> IO ()
runit name = do         
  eprog <- validate <$> parseFile name
  case eprog of
    Left e -> print e
    Right prog ->
      let (vec, lmap) = remap prog
      in do
        putStrLn $ pretty vec
        runProg vec lmap
         
      
