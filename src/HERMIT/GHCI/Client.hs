{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module HERMIT.GHCI.Client where

import Control.Monad (void, when)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Control.Natural
import qualified Control.Remote.Monad.JSON as JSONRPC
import Control.Remote.Monad.JSON.Trace
import Control.Remote.Monad.JSON.Client
import Control.Remote.Monad.JSON.Types (SendAPI(..), Args(..))

import HERMIT.API.Types
import HERMIT.GHCI.Types
import HERMIT.Debug (debug)
import HERMIT.GHCI.JSON
import HERMIT.GHCI.Renderer

import Data.IORef (readIORef)


-- For better error messages
import Data.Text (Text, unpack)
import Data.Vector (toList)

--- Main call-HERMIT function

session :: JSONRPC.Session
session = JSONRPC.weakSession $ tracer $clientSendAPI "http://localhost:3000/"
 where
    tracer :: (SendAPI :~> IO) -> SendAPI :~> IO
    tracer = if debug
             then traceSendAPI "HERMIT-remote-json"
             else id

send :: Shell a -> IO a
send (Return a) = return a
send (Bind m k) = send m >>= send . k
send (Shell g) = do
       when debug $ print g
       v <- JSONRPC.send session $ JSONRPC.method "send" $ List [g]
       case fromJust $ parseMaybe parseJSON v of
         -- Normal shell behavior; something like variable not found
         ShellException msg -> do
             print g
             error $ "server failure: " ++ show v ++ " : " ++ msg
         -- Internal Failure; bad news
         ShellFailure msg ->
             error $ "failed to parse result value for " ++
                     genMethodStr True g ++ ": " ++ show v ++ " : " ++ msg
         ShellResult gss a ->
             do mapM printResponse gss
                return a
send (Local m) = m
send (Fail str) = fail str

genMethodStr :: Bool -> Value -> String
genMethodStr fl (Object o) =
  case parseMaybe (\ x -> x .: "method") o :: Maybe Text of
    Nothing -> "(Object { " ++ show o ++ "})"
    Just mthd ->
        let prms = fromJust $ parseMaybe (\ x -> x .: "params") o :: [Value]
            prms' = map (genMethodStr False) (reverse prms)
            wrap :: String -> String
            wrap "" = ""
            wrap str = if fl then " (" ++ str ++ ")" else " " ++ str in
          unpack mthd ++ wrap (unwords prms')
genMethodStr _ (Array vec) =
    show . map (genMethodStr False) $ toList vec
genMethodStr _ (String str) = unpack str
genMethodStr _ (Number n) = show n
genMethodStr _ (Bool b) = show b
genMethodStr _ Null = ""

