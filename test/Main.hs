module Main where

import Ast
import Parser
import Test.HUnit
import Prelude hiding (id)

main :: IO ()
main = runTestTTAndExit tests

parseTerm :: String -> Term -> Test
parseTerm s t = TestCase (assertEqual ("Parse: " ++ s) (Right [t]) (parse s))

parseProgram :: String -> [Term] -> Test
parseProgram s ts = TestCase (assertEqual ("Parse: " ++ s) (Right ts) (parse s))

id :: Term
id = Lambda (Universe Z) (Lambda (Var Z) (Var Z))

foo :: Term
foo = Pi (Universe Z) (Pi (Pi (Universe Z) (Pi (Var Z) (Var (S Z)))) (Var (S Z)))

bottom :: Term
bottom = App foo (Pi (Universe Z) (Var Z))

tests :: Test
tests =
  TestList
    [ -- Lambda
      parseTerm "\\(A : *). *" (Lambda (Universe Z) (Universe Z)),
      parseTerm "\\(A : *) (x : A). x" id,
      parseTerm "\\(A : *) (x : A) (y : A). x" (Lambda (Universe Z) (Lambda (Var Z) (Lambda (Var (S Z)) (Var (S Z))))),
      -- Decl
      parseTerm "id := \\(A : *) (x : A). x" id,
      parseTerm "id (A : *) (x : A) := x" id,
      parseTerm "foo := (T : *) -> ((A : *) -> (x : A) -> A) -> T" foo,
      parseProgram "foo := (T : *) -> ((A : *) -> (x : A) -> A) -> T  \n  id := \\(A : *) (x : A). x" [foo, id],
      -- Pi
      parseTerm "(A : *) -> A" (Pi (Universe Z) (Var Z)),
      parseTerm "(A : *) -> (B : *) -> A" (Pi (Universe Z) (Pi (Universe Z) (Var (S Z)))),
      parseTerm "(A : *) -> (B : *) -> B" (Pi (Universe Z) (Pi (Universe Z) (Var Z))),
      parseTerm "(A : *) -> (x : A) -> A" (Pi (Universe Z) (Pi (Var Z) (Var (S Z)))),
      parseTerm "(A : *) -> (B : (_ : *) -> *) -> B A" (Pi (Universe Z) (Pi (Pi (Universe Z) (Universe Z)) (App (Var Z) (Var (S Z))))),
      parseTerm "(T : *) -> (_ : (A : *) -> (x : A) -> A) -> T" foo,
      parseTerm "(T : *) -> ((A : *) -> (x : A) -> A) -> T" foo,
      -- App
      parseTerm "(\\(A : *)(x : A).x) (\\(A : *).A)" (App id (Lambda (Universe Z) (Var Z)))
    ]
