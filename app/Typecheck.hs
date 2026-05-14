module Typecheck where

import Data.Fin
import Control.Monad
import Control.Monad.Error.Class

import Rebound.Context ((+++))
import Rebound.MonadScoped
import Rebound.Env
import Rebound.Bind.Local (instantiate1, unbindl1)

import Syntax
import Environment

-- Helper functions
lookupTy :: Fin n -> TCMonad n (Type n)
lookupTy x = readerS (\(Context e) -> applyEnv e x)

extendTy :: Type n -> Context n -> Context (S n)
extendTy d (Context gamma) = Context (gamma +++ d)

-- Typechckers
inferType :: Term n -> TCMonad n (Type n)
inferType term = case term of
    -- I-Var
    Var n -> lookupTy n
    -- I-App-Simple
    App t1 t2 -> do
        ty_t1 <- inferType t1
        case ty_t1 of
            TyPi ty_arg body -> do
                checkType t2 ty_arg
                return $ instantiate1 body t2
            _ -> throwError $ "I-App-Simple: callee is not a function: " ++ show term
    -- I-Pi 
    TyPi ty_arg body -> do
        checkType ty_arg TyType
        localS (extendTy ty_arg) (checkType (snd $ unbindl1 body) TyType)
        return TyType
    -- I-Type
    TyType -> return TyType
    -- I-Annot
    Ann t ty -> do
        checkType ty TyType
        checkType t ty
        return ty
    -- Type literals
    TyUnit    -> return TyType
    LitUnit   -> return TyUnit
    TyBool    -> return TyType
    LitBool _ -> return TyBool
    -- Otherwise, error
    _ -> throwError $ "inferType: Need type annotation at: " ++ show term

checkType :: Term n -> Type n -> TCMonad n ()
checkType t ty = case t of
    Lam body -> case ty of
        TyPi arg_ty body_ty -> do
            let body' = snd $ unbindl1 body
            let body_ty' = snd $ unbindl1 body_ty
            localS (extendTy arg_ty) (checkType body' body_ty')
        _ -> throwError $ 
            "checkType: lambda (" ++ show t ++ ") should be functions and not (" ++ show ty ++ ")."
    _ -> do
        ty' <- inferType t
        unless (ty == ty') (throwError $
            "checkType: typecheck failed for (" ++ show t ++ " : " ++ show ty ++ ")")
