module Syntax where

import Data.Fin

import Rebound.Bind.Local hiding (Type)

data Term (n :: Nat)
    = -- | type of types `Type`
    TyType
    | -- | variables `x`
    Var (Fin n)
    | -- | abstractions `λx. a`
    Lam (Bind1 Term Term n)
    | -- | applications `a b`
    App (Term n) (Term n)
    | -- | function types `(x : A) → B`
    TyPi (Type n) (Bind1 Term Term n)
    | -- | Annotated terms `( a : A )`
    Ann (Term n) (Type n)

deriving instance (Generic1 Term)

type Type = Term

instance SubstVar Term where
  var :: Fin n -> Term n
  var = Var

instance Shiftable Term where
  shift = shiftFromApplyE @Term

instance Subst Term Term where
  isVar (Var x) = Just (Refl, x)
  isVar _ = Nothing

-- Alpha-equivalence
instance Eq (Term n) where
    term1 == term2 = case (term1, term2) of 
        (TyType, TyType) -> True
        (Var x, Var y) -> x == y
        (Lam s, Lam t) -> s == t
        (App s1 s2, App t1 t2) -> (s1 == t1) && (s2 == t2)
        (TyPi x s, TyPi y t) -> (x == y) && (s == t)
        (Ann x s, Ann y t) -> x == y && s == t
        (_, _) -> False
