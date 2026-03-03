module Parser where

import Ast
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Reader
import Data.Functor
import Text.Parsec hiding (parse)

type Scope = [String]

newtype UnboundVariable = UnboundVariable String deriving (Show, Eq)

type Parser = ParsecT String () (ReaderT Scope (Either UnboundVariable))

unboundVariable :: String -> Parser a
unboundVariable = liftEither . Left . UnboundVariable

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

parens :: Parser a -> Parser a
parens p = lexeme (char '(') *> p <* lexeme (char ')')

universe :: Parser Term
universe = lexeme $ char '*' $> Universe Z -- TODO

binding :: Parser String
binding = lexeme $ many1 (lower <|> upper <|> char '_')

find :: String -> Parser Term
find s = do
  scope <- ask
  case go Z scope of
    Just n -> return (Var n)
    Nothing -> unboundVariable s
  where
    go _ [] = Nothing
    go n (b : bs) = if b == s then Just n else go (S n) bs

var :: Parser Term
var = do
  b <- binding
  find b

bind :: String -> Parser ()
bind b = local (b :) (pure ())

term :: Parser Term
term =
  choice
    [ try decl,
      try lambda,
      try pi,
      universe,
      var,
      app,
      parens term
    ]
  where
    arg = lexeme $ parens do
      b <- binding
      _ <- lexeme (char ':')
      t <- term
      _ <- bind b
      return t
    pi = do
      t <- arg
      _ <- lexeme $ string "->"
      Pi t <$> term
    lambda = do
      _ <- lexeme $ char '\\'
      args <- many arg -- wrong, args can depend on each other
      _ <- lexeme $ char '.'
      body <- term
      return $ foldr Lambda body args
    atom =
      choice
        [ universe,
          var,
          parens term
        ]
    app = do
      f <- atom
      args <- many1 atom
      return $ foldl App f args
    decl = do
      b <- binding
      bind b
      args <- many arg
      _ <- lexeme (string ":=")
      body <- term
      return $ foldr Lambda body args

data Error
  = ParseError ParseError
  | UnboundVar UnboundVariable
  deriving (Show, Eq)

parse :: String -> Either Error Term
parse s = case runReaderT (runPT term () "" s) [] of
  Left e -> Left (UnboundVar e)
  Right (Left e) -> Left (ParseError e)
  Right (Right t) -> Right t
