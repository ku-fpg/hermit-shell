{-# LANGUAGE OverloadedStrings #-}
module HERMIT.GHCI.Client where

import Control.Lens ((^.))
import Control.Monad (void, when)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Control.Monad.Remote.JSON as JSONRPC
import Network.Wreq
import HERMIT.API.Types
import HERMIT.Debug (debug)
import HERMIT.GHCI.JSON

import System.Console.ANSI

--- Main call-HERMIT function

session :: JSONRPC.Session
session = JSONRPC.defaultSession JSONRPC.Weak (\ v -> do
          r <- asJSON =<< post "http://localhost:3000/" (toJSON v)
          return $ r ^. responseBody)
   (\ v -> do
          void $ post "http://localhost:3000/" (toJSON v)
          return ())

send :: Shell a -> IO a
send (Return a) = return a
send (Bind m k) = send m >>= send . k
send (Shell g) = do
       when debug $ print g
       v <- JSONRPC.send session $ JSONRPC.method "send" [g]
       case fromJust $ parseMaybe parseJSON v of
         ShellException msg ->
             error $ "server failure: " ++ show v ++ " : " ++ msg
         ShellFailure msg -> error $ "failed to parse result value: " ++ show v ++ " : " ++ msg
         ShellResult gss a -> do
                 sequence_ [ withStyle sty txt
                           | gs <- gss
                           , Glyph txt sty <- gs
                           ]
                 putStrLn "\n[Done]\n"
                 return a
send (Fail str) = fail str

withStyle :: Maybe Style -> String -> IO ()
withStyle Nothing    str = putStr str
withStyle (Just sty) str = do
  setSGR $ maybe [] (\x -> [x]) (styleSGR sty)
  putStr str
  setSGR [Reset]

styleSGR :: Style -> Maybe SGR
styleSGR KEYWORD  = Just $ simpleColor Blue
styleSGR SYNTAX   = Just $ simpleColor Red
styleSGR VAR      = Nothing
styleSGR COERCION = Just $ simpleColor Yellow
styleSGR TYPE     = Just $ simpleColor Green
styleSGR LIT      = Just $ simpleColor Cyan
styleSGR WARNING  = Just $ SetColor Background Vivid Yellow

simpleColor :: Color -> SGR
simpleColor = SetColor Foreground Vivid

