{-# LANGUAGE TupleSections #-}

module Parser where

import Ast
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Reader
import Text.Parsec hiding (parse)

type Scope = [String]

newtype UnboundVariable = UnboundVariable String deriving (Show, Eq)

type Parser = ParsecT String () (Either UnboundVariable)

unboundVariable :: String -> Parser a
unboundVariable = liftEither . Left . UnboundVariable

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

parens :: Parser a -> Parser a
parens p = lexeme (char '(') *> p <* lexeme (char ')')

universe :: Parser Term
universe = Universe . lengthNat <$> lexeme (char '*' *> many (char '*'))

binding :: Parser String
binding = lexeme $ many1 (lower <|> upper <|> char '_')

type WithScope a = Scope -> Parser a

find :: String -> WithScope Term
find s scope = do
  case go Z scope of
    Just n -> return (Var n)
    Nothing -> unboundVariable s
  where
    go _ [] = Nothing
    go n (b : bs) = if b == s then Just n else go (S n) bs

var :: WithScope Term
var scope = do
  b <- binding
  find b scope

arg :: WithScope (String, Term)
arg scope = lexeme $ parens do
  b <- binding <* lexeme (char ':') <|> return "_"
  t <- term scope
  return (b, t)

-- args :: String -> Parser a -> Parser (a, [Term])
-- args sep p = do
--   (b, t) <- arg
--   bind b (args' [t])
--   where
--     args' ts =
--       (string sep >> fmap (,ts) p) <|> do
--         (b', t') <- arg
--         bind b' (args' (ts ++ [t']))

chainArgs :: Scope -> Parser (Scope, [Term])
chainArgs scope =
  ( do
      (b, t) <- arg scope
      (scope', ts) <- chainArgs (b : scope)
      return (scope', t : ts)
  )
    <|> return (scope, [])

term :: WithScope Term
term scope =
  choice
    [ try (decl scope),
      try (lambda scope),
      try (pi scope),
      universe,
      var scope,
      app scope,
      parens (term scope)
    ]
  where
    pi scope = do
      (b, t) <- arg scope
      _ <- lexeme $ string "->"
      Pi t <$> term (b : scope)
    lambda scope = do
      _ <- lexeme $ char '\\'
      (scope', ts) <- chainArgs scope
      _ <- lexeme $ char '.'
      body <- term scope'
      return $ foldr Lambda body ts
    decl scope = do
      b <- binding
      (scope', ts) <- chainArgs (b : scope)
      _ <- lexeme $ string ":="
      body <- term scope'
      return $ foldr Lambda body ts
    atom scope =
      choice
        [ universe,
          var scope,
          parens (term scope)
        ]
    app scope = do
      f <- atom scope
      args <- many1 (atom scope)
      return $ foldl App f args

data Error
  = ParseError ParseError
  | UnboundVar UnboundVariable
  deriving (Show, Eq)

parse :: String -> Either Error Term
parse s = case runPT (term []) () "" s of
  Left e -> Left (UnboundVar e)
  Right (Left e) -> Left (ParseError e)
  Right (Right t) -> Right t
