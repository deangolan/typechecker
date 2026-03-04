module TypeChecker where

import Ast
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Reader
import Eval

type Context = [Term]

type TypeChecking = ReaderT Context (Either TypeError)

data TypeError
  = CannotApply
      { term :: Term,
        ofType :: Term,
        toFunction :: Term,
        withType :: Term,
        andDomain :: Term
      }
  | NotAFunction {cannotApply :: Term, to :: Term, withType :: Term}
  | UnboundVariable Nat
  | InvalidDomainInPi {domain :: Term, withType :: Term}
  | InvalidCodomainInPi {codomain :: Term, withType :: Term}
  deriving (Show)

throw :: TypeError -> TypeChecking a
throw = liftEither . Left

find :: Nat -> TypeChecking Term
find n = do
  ctx <- ask
  either throw pure (find' ctx n)
  where
    find' [] n = Left (UnboundVariable n)
    find' (t : _) Z = Right t
    find' (_ : ts) (S n) = find' ts n

bind :: Term -> TypeChecking a -> TypeChecking a
bind t = local (t :)

-- should never return term that is a value
typecheck :: Term -> TypeChecking Term
typecheck = \case
  Universe n -> return (Universe (S n))
  Var n -> find n
  Lambda dom body -> do
    _ <- typecheck dom
    cod <- bind dom (typecheck body)
    return (Pi dom cod)
  Pi dom cod -> do
    dt <- typecheck dom
    ct <- bind dom (typecheck cod)
    case (dt, ct) of
      (Universe n, Universe m) -> return (Universe (max n m))
      (Universe _, _) -> throw (InvalidDomainInPi cod dt)
      _ -> throw (InvalidDomainInPi dom dt)
  App f t -> do
    fTy <- typecheck f
    case normalize fTy of
      Pi dom cod -> do
        tTy <- typecheck t
        if normalize tTy == normalize dom
          then return cod
          else throw (CannotApply t tTy f fTy dom)
      _ -> throw (NotAFunction t f fTy)

typechecker :: [Term] -> Either TypeError [Term]
typechecker terms = runReaderT (traverse typecheck terms) []
