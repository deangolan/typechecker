{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Ast where

data Term
  = Universe Nat
  | Var Nat
  | Pi Term Term
  | Lambda Term Term
  | App Term Term
  deriving (Show, Eq)

data Nat = Z | S Nat deriving (Eq, Ord, Show)

lengthNat :: [a] -> Nat
lengthNat [] = Z
lengthNat (_ : xs) = S (lengthNat xs)

-- TODO: Ideally we could get rid of unbound variables during parsing and return
-- an ast that cannot have unbound variables by its type.
-- This type works but it was too complicated to construct during parsing.
--
-- data Index numArgs where
--   IZ :: Index n
--   IS :: Index n -> Index ('S n)
--
-- data Expr' numArgs where
--   Universe :: Nat -> Expr' n
--   Var :: Index n -> Expr' ('S n)
--   Pi :: Expr -> Expr' ('S n) -> Expr' n
--   Lambda :: Expr -> Expr' ('S n) -> Expr' n
--   App :: Expr -> Expr -> Expr' n
--
-- data Expr where
--   Expr :: Expr' Z -> Expr
