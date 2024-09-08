{-# LANGUAGE FlexibleInstances #-}

module Monad
    ( TDM, runTDM
    , unwrap, ignore
    , Checkable, check
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent.STM.TQueue
import qualified TD.Lib as TDL
import qualified TD.GeneralResult as TDL

type TDM = ReaderT (TDL.Client, TQueue TDL.GeneralResult) (ExceptT () IO)

runTDM :: TDM a -> TDL.Client -> TQueue TDL.GeneralResult -> IO (Maybe a)
runTDM m client q = either (const Nothing) Just <$> runExceptT (runReaderT m (client, q))

unwrap :: Maybe a -> TDM a
unwrap = mapError $ maybe mzero pure

ignore :: TDM a -> TDM ()
ignore = void . tryError

class Checkable a where
    check :: a -> TDM ()

instance Checkable Bool where
    check = guard

instance Checkable (Maybe Bool) where
    check = unwrap >=> guard
