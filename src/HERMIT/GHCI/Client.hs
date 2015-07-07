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
  setSGR $ styleSGR sty
  putStr str
  setSGR [Reset]

styleSGR :: Style -> [SGR]
styleSGR KEYWORD  = [simpleColor Blue]
styleSGR SYNTAX   = [simpleColor Red]
styleSGR VAR      = []
styleSGR COERCION = [simpleColor Yellow]
styleSGR TYPE     = [simpleColor Green]
styleSGR LIT      = [simpleColor Cyan]
styleSGR WARNING  = [SetColor Background Vivid Yellow
                    ,SetColor Foreground Dull  Black
                    ]

simpleColor :: Color -> SGR
simpleColor = SetColor Foreground Vivid

