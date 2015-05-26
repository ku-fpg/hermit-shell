{-# LANGUAGE OverloadedStrings #-}
module HERMIT.GHCI.Client where

import Control.Concurrent.STM
import System.IO.Unsafe
import Control.Lens
import Data.Aeson (toJSON, encode, Value)
import Data.Aeson.Lens
import Data.ByteString.Lazy (ByteString)
import Network.Wreq

import HERMIT.GHCI.JSON

-- Call once at start. Sets up session for this GHCi process.
-- There can only be one session per GHCi process.


initalize :: IO ()
initalize = do
  r <- asJSON =<< post "http://localhost:3000/connect" ("" :: ByteString)
  atomically $ writeTVar globalSessionState $ Just $ GlobalSessionState
          { gss_token = r ^. responseBody
          }
  return ()

{-# NOINLINE globalSessionState #-}
globalSessionState :: TVar (Maybe GlobalSessionState)
globalSessionState = unsafePerformIO $ newTVarIO $ Nothing
        
data GlobalSessionState = GlobalSessionState
  { gss_token :: Token -- User Id, and which AST (id) we are looking at.
  } 
  deriving Show

--data ClientRequest = ClientRequest Token Value
--data ClientResponse = ClientRequest 

-- Send 
getGlobalSessionState :: IO GlobalSessionState
getGlobalSessionState = atomically $ do
     g <- readTVar globalSessionState
     case g of
       Nothing -> retry
       Just v -> return v

send :: Value -> IO Value
send inp = do
  glob <- getGlobalSessionState
  print glob               
  r <- asJSON =<< post "http://localhost:3000/send" (encode (gss_token glob,inp))
  print (r ^. responseBody :: Value)
--  setGlobalSessionState $ glob { 
  return inp