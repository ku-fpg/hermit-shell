{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module HERMIT.GHCI.Client where

import Control.Lens ((^.))
import Control.Monad (void, when)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Control.Monad.Remote.JSON as JSONRPC
import Control.Monad.Remote.JSON.Debug
import Control.Monad.Remote.JSON.Types (SessionAPI(..), Args(..))
import Network.Wreq
import HERMIT.API.Types
import HERMIT.Debug (debug)
import HERMIT.GHCI.JSON
import HERMIT.GHCI.Glyph


-- For better error messages
import Data.Text (Text, unpack)
import Data.Vector (toList)

--- Main call-HERMIT function

session :: JSONRPC.Session
session = JSONRPC.session
        $ (if debug 
           then traceSessionAPI "HERMIT-remote-json"
           else id)
        $ sendr
 where
        sendr :: SessionAPI a -> IO a
        sendr (Sync v) =  do
          r <- asJSON =<< post "http://localhost:3000/" (toJSON v)
          return $ r ^. responseBody
        sendr (Async v) = do
          void $ post "http://localhost:3000/" (toJSON v)


send :: Shell a -> IO a
send (Return a) = return a
send (Bind m k) = send m >>= send . k
send (Shell g) = do
       when debug $ print g
       v <- JSONRPC.send session $ JSONRPC.method "send" $ List [g]
       case fromJust $ parseMaybe parseJSON v of
         -- Normal shell behavior; something like variable not found
         ShellException msg ->
             error $ "server failure: " ++ show v ++ " : " ++ msg
         -- Internal Failure; bad news
         ShellFailure msg ->
             error $ "failed to parse result value for " ++
                     genMethodStr True g ++ ": " ++ show v ++ " : " ++ msg
         ShellResult gss a -> do
                 sequence_ [ withNoStyle sty txt
                           | gs <- gss
                           , Glyph txt sty <- gs
                           ]
                 putStrLn "\n[Done]\n"
                 return a
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

