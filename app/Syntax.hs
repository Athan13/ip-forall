module Syntax where

import Data.Fin

import Rebound.Bind.Local hiding (Type)

data Term (n :: Nat)
    = -- type of types `Type`
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
    | -- | the type with a single inhabitant, called `Unit`
    TyUnit
    | -- | the inhabitant of `Unit`, written `()`
    LitUnit
    | -- | the type with two inhabitants `Bool`
    TyBool
    | -- | `True` and `False`
    LitBool Bool
    | -- | if b then a1 else a2
    If (Type n) (Type n) (Type n)

deriving instance (Generic1 Term)

instance Show (Term n) where
  show term = case term of
    TyType -> "Type"
    Var n -> "Var " ++ show n
    Lam _ -> "Lam"
    App t1 t2 -> "App (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
    TyPi _ _ -> "TyPi"
    Ann t ty -> "Ann (" ++ show t ++ " : " ++ show ty ++ " )"
    TyUnit -> "Unit"
    LitUnit -> "()"
    TyBool -> "Bool"
    LitBool b -> show b
    If a b1 b2 -> "If (" ++ show a ++ ") then (" ++ show b1 ++ ") else (" ++ show b2

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
        (App s_1 s_2, App t_1 t_2) -> (s_1 == t_1) && (s_2 == t_2)
        (TyPi x s, TyPi y t) -> (x == y) && (s == t)
        (Ann x s, Ann y t) -> x == y && s == t
        (TyUnit, TyUnit) -> True
        (LitUnit, LitUnit) -> True
        (TyBool, TyBool) -> True
        (LitBool b1, LitBool b2) -> b1 == b2
        (If a b1 b2, If c d1 d2) -> (a == c) && (b1 == d1) && (b2 == d2)
        (_, _) -> False
