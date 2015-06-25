{-# LANGUAGE OverloadedStrings #-}
module HERMIT.GHCI.Client where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Control.Monad.Remote.JSON (Session(..))
import qualified Control.Monad.Remote.JSON as JSONRPC
import Network.Wreq
import HERMIT.GHCI.JSON
import HERMIT.API.Types

--- Main call-HERMIT function

session :: JSONRPC.Session
session = defaultSession Weak (\ v -> do
          r <- asJSON =<< post "http://localhost:3000/" (toJSON v)
          return $ r ^. responseBody)
   (\ v -> do
          void $ post "http://localhost:3000/" (toJSON v)
          return ())

send :: Shell a -> IO a
send (Return a) = return a
send (Bind m k) = send m >>= send . k
send (Shell g) = do
       print g
       v <- JSONRPC.send session $ JSONRPC.method "send" [g]
       case fromJust $ parseMaybe parseJSON v of
         ShellException msg -> 
             error $ "server failure: " ++ show v ++ " : " ++ msg
         ShellFailure msg -> error $ "failed to parse result value: " ++ show v ++ " : " ++ msg
         ShellResult gss a -> do
                 sequence_ [ putStr txt
                           | gs <- gss
                           , Glyph txt _ <- gs
                           ]
                 putStrLn "\n[Done]\n"
                 return a
send (Fail str) = fail str

