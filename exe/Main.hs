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
      do
        let ast = parse contents
        _ <- print ast
        case typechecker . pure <$> ast of
          Left parseErr -> print $ "Parsing error: " ++ show parseErr
          Right (Left typeErr) -> print $ "Invalid type: " ++ show typeErr
          Right (Right terms) -> print terms
