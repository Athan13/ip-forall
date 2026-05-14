module Main where

import Data.Fin
import Control.Monad.Except (runExcept)

import Rebound.Bind.Local (bind1, LocalName (LocalName))

import Syntax
import Typecheck (inferType)
import Environment (runTCMonad)
import Data.List (intercalate)

-- The identity function for types: λx. x (or, in de Bruijn notation, λ. 0)
e0 :: Term Z
e0 = Lam (LocalName "x" `bind1` Var 0)

-- The identity function for values: λ(y : Type). λ(x : y). x (or, in de Bruijn, λ. λ. 0)
e1 :: Term Z
e1 = Lam (LocalName "y" `bind1` Lam (LocalName "x" `bind1` Var 0))

examples :: [Term Z]
examples = [e0, e1]

typeCheckExpr :: Term Z -> String
typeCheckExpr term = case runExcept $ runTCMonad $ inferType term of
    Left err -> "Error: " ++ err
    Right ty -> "Typechecked! Expression has type: " ++ show ty

main :: IO ()
main = putStrLn $ intercalate "\n" 
    ["ex" ++ show (n :: Int) ++ ": " ++ typeCheckExpr s | (n, s) <- zip [0..] examples]
