module TDM
    ( start, auth
    , get, put
    , send
    , module Monad
    , module TD.GeneralResult
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Data.Text.Lens
import Data.Aeson (ToJSON)
import System.Environment
import qualified TD.Lib as TDL
import TD.GeneralResult
import TD.Data.Update
import TD.Data.AuthorizationState
import TD.Query.SetTdlibParameters
import TD.Query.SetAuthenticationPhoneNumber
import TD.Query.CheckAuthenticationCode

import Monad

send :: ToJSON a => a -> TDM ()
send x = view _1 >>= \client -> liftIO $ TDL.send client x

recv :: TDM (Maybe (GeneralResult, Maybe TDL.Extra))
recv = view _1 >>= liftIO . TDL.receive

get :: TDM GeneralResult
get = view _2 >>= liftIO . atomically . readTQueue

put :: GeneralResult -> TDM ()
put x = view _2 >>= \q -> liftIO $ atomically $ writeTQueue q x

recvloop :: TDM ()
recvloop = forever $ ignore $ do
    recv >>= unwrap >>= put . fst
    liftIO $ threadDelay 1000

start :: TDM a -> IO ()
start m = do
    client <- TDL.create 
    q <- atomically newTQueue

    forkIO $ void $ runTDM recvloop client q
    void $ runTDM m client q

auth :: TDM ()
auth = do
    x <- get
    case x of
        Update (UpdateAuthorizationState (Just AuthorizationStateReady)) -> pure ()
        Update (UpdateAuthorizationState (Just state)) -> exec state >> auth
        _ -> put x >> auth

exec :: AuthorizationState -> TDM ()

exec AuthorizationStateReady = pure ()

exec AuthorizationStateWaitTdlibParameters = do
    apiId <- liftIO $ getEnv "API_ID"
    apiHash <- liftIO $ getEnv "API_HASH"
    send $ defaultSetTdlibParameters { database_directory = Just $ "db" ^. packed
                                     , api_id = Just $ read apiId
                                     , api_hash = Just $ apiHash ^. packed
                                     , device_model = Just $ "Haskell" ^. packed
                                     , system_language_code = Just $ "en" ^. packed
                                     , application_version = Just $ "1.0.0" ^. packed
                                     }

exec AuthorizationStateWaitPhoneNumber = do
    phone <- liftIO $ getEnv "PHONE_NUMBER"
    send $ defaultSetAuthenticationPhoneNumber { phone_number = Just $ phone ^. packed }

exec (AuthorizationStateWaitCode _) = do
    liftIO $ putStrLn "Type authentication code"
    code <- liftIO $ getLine
    send $ CheckAuthenticationCode { code = Just $ code ^. packed }

exec x = do
    liftIO $ putStrLn $ "Unknown Authorization State:\n" ++ show x
    mzero
