module Parser where

import Ast
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Reader
import Text.Parsec hiding (parse)

type Scope = [String]

newtype UnboundVariable = UnboundVariable String deriving (Show, Eq)

type Parser = ParsecT String () (ReaderT Scope (Either UnboundVariable))

unboundVariable :: String -> Parser a
unboundVariable = liftEither . Left . UnboundVariable

lexeme :: Parser a -> Parser a
lexeme p = p <* skipMany (char ' ')

parens :: Parser a -> Parser a
parens p = lexeme (char '(') *> p <* lexeme (char ')')

universe :: Parser Term
universe = Universe . lengthNat <$> lexeme (char '*' *> many (char '*'))

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

arg :: Parser (String, Term)
arg = lexeme $ parens do
  b <- binding <* lexeme (char ':') <|> return "_"
  t <- term
  return (b, t)

args :: Parser (Scope, [Term])
args =
  ( do
      (b, t) <- arg
      (scope', ts) <- local (b :) args
      return (scope', t : ts)
  )
    <|> ( do
            scope <- ask
            return (scope, [])
        )

term :: Parser Term
term =
  choice
    [ try decl,
      try lambda,
      try pi,
      try app,
      universe,
      var,
      parens term
    ]
  where
    pi = do
      (b, t) <- arg
      _ <- lexeme $ string "->"
      Pi t <$> local (b :) term
    lambda = do
      _ <- lexeme $ char '\\'
      (b, t) <- arg
      (scope', ts) <- local (b :) args
      _ <- lexeme $ char '.'
      body <- local (const scope') term
      return $ foldr Lambda body (t : ts)
    decl = do
      b <- binding
      (scope', ts) <- local (b :) args
      _ <- lexeme $ string ":="
      body <- local (const scope') term
      return $ foldr Lambda body ts
    atom =
      choice
        [ universe,
          var,
          parens term
        ]
    app = do
      f <- atom
      ts <- many1 atom
      return $ foldl App f ts

data Error
  = ParseError ParseError
  | UnboundVar UnboundVariable
  deriving (Show, Eq)

parse :: String -> Either Error [Term]
parse s = case runReaderT (runPT (many (term <* spaces)) () "" s) [] of
  Left e -> Left (UnboundVar e)
  Right (Left e) -> Left (ParseError e)
  Right (Right t) -> Right t
