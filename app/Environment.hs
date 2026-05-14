module Environment where

import Data.Fin

import Control.Monad.Except

import qualified Rebound (Ctx)
import Rebound.MonadScoped (ScopedReaderT (runScopedReaderT), MonadScopedReader)
import Rebound (emptyC)

import Syntax

newtype Context (n :: Nat) = Context (Rebound.Ctx Type n)

newtype TCMonad (n :: Nat) a = TCMonad (ScopedReaderT Context (Except String) n a)
    deriving (Functor, Applicative, Monad, MonadError String)
deriving instance (MonadScopedReader Context TCMonad)

runTCMonad :: TCMonad Z a -> Except String a
runTCMonad (TCMonad m) = runScopedReaderT m (Context emptyC)
