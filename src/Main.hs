module Main where

import           Lexer
import           Evaluator
import           Data.Maybe

main :: IO ()
main = do
  s <- getLine
  let ast = strToAST s
  if isJust ast
    then putStrLn ((show . evaluate) (fromJust ast))
    else putStrLn "ERROR"
  return ()
