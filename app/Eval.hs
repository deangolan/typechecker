module Eval where

import Ast

shift :: Nat -> Term -> Term
shift cutoff = \case
  Universe n -> Universe n
  Var n -> if n >= cutoff then Var (S n) else Var n
  Pi ty body -> Pi ty (shift (S cutoff) body)
  Lambda ty body -> Lambda ty (shift (S cutoff) body)
  App t1 t2 -> App (shift cutoff t1) (shift cutoff t2)

substitute :: Term -> Term -> Term
substitute = go Z
  where
    go depth binding = \case
      Universe n -> Universe n
      Var n -> if depth == n then binding else Var n
      Pi ty body -> Pi ty (go (S depth) (shift depth binding) body)
      Lambda ty body -> Lambda ty (go (S depth) (shift depth binding) body)
      App t1 t2 -> App (go depth binding t1) (go depth binding t2)

reduce :: Term -> Term
reduce (App (Lambda _ body) t) = substitute t body
reduce (App t1 t2) = App (reduce t1) (reduce t2)
reduce (Pi ty body) = Pi ty (reduce body)
reduce (Lambda ty body) = Lambda ty (reduce body)
reduce (Var v) = Var v
reduce (Universe n) = Universe n

normalize :: Term -> Term
normalize t =
  let t' = reduce t
   in if t' == t then t else normalize t'
