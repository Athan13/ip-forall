module Main where

import Data.Fin
import Control.Monad.Except (runExcept)

import Rebound.Bind.Local (bind1, LocalName (LocalName))

import Syntax
import Typecheck (inferType)
import Environment (runTCMonad)
import Data.List (intercalate)

-- The identity function for types: `λ(x : Type). x`
-- In de Bruijn notation, `λ. 0`
ex0 :: Term Z
ex0 = Ann
    (Lam (LocalName "x" `bind1` Var 0))
    (TyPi TyType (LocalName "x" `bind1` TyType))

-- The identity function for values: `λ(y : Type). λ(x : y). x`
-- In de Bruijn: `λ. λ. 0`
ex1 :: Term Z
ex1 = Ann
    (Lam (LocalName "y" `bind1` Lam (LocalName "x" `bind1` Var 0)))
    (TyPi TyType (LocalName "y" `bind1`
        TyPi (Var 0) (LocalName "x" `bind1` Var 1)
    ))

-- The `and` function on booleans: `λ(x : Bool). λ(y : Bool) if x then y else False`
-- In de Bruijn: `λ. λ. if 1 then 0 else False`
ex2 :: Term Z
ex2 = Ann
    (Lam (LocalName "x" `bind1` 
        Lam (LocalName "y" `bind1` 
            If (Var 1) (Var 0) (LitBool False))))
    (TyPi TyBool (LocalName "x" `bind1`
        TyPi TyBool (LocalName "y" `bind1` TyBool)
    ))

examples :: [Term Z]
examples = [ex0, ex1, ex2]

typeCheckExpr :: Term Z -> String
typeCheckExpr term = case runExcept $ runTCMonad $ inferType term of
    Left err -> "Error: " ++ err
    Right ty -> "Typechecked! Expression has type: " ++ show ty

main :: IO ()
main = putStrLn $ intercalate "\n" 
    ["ex" ++ show (n :: Int) ++ ": " ++ typeCheckExpr s | (n, s) <- zip [0..] examples]
