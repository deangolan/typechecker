module Main where

import Parser (parse)
import System.Environment (getArgs)
import TypeChecker (typechecker)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "No input file provided."
    path : _ -> do
      contents <- readFile path
      _ <- print contents
      case typechecker . pure <$> parse contents of
        Left parseErr -> print "Parsing error: " >> print parseErr
        Right (Left typeErr) -> print "Invalid type: " >> print typeErr
        Right (Right terms) -> print terms
