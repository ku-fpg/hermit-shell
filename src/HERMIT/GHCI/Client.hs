{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module HERMIT.GHCI.Client where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Aeson
import Control.Monad.Remote.JSON as JSONRPC
import Network.Wreq
import HERMIT.GHCI.JSON 
import HERMIT.API.Types

--- Main call-HERMIT function
        
session :: JSONRPC.Session
session = Session 
  { sync = \ v -> do
          r <- asJSON =<< post "http://localhost:3000/" (toJSON v)
          return $ r ^. responseBody
  , async = \ v -> do
          void $ post "http://localhost:3000/" (toJSON v)
          return ()
  }        

send :: Shell a -> IO a
send g = do
       print (toShell g)
       v <- JSONRPC.send session $ JSONRPC.method "send" [toShell g]
       case fromShell g v of
         ShellFailure msg -> error $ "failed to parse result value: " ++ show v ++ " : " ++ msg
         ShellResult gss a -> do
                 sequence_ [ putStr txt
                           | gs <- gss
                           , Glyph txt _ <- gs
                           ]
                 putStrLn "\n[Done]\n"           
                 return a
