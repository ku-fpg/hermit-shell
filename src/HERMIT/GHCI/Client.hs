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
import HERMIT.GHCI.Glyph


-- For better error messages
import Data.Text (Text, unpack)
import Data.Vector (toList)

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
         ShellFailure msg -> 
             error $ "failed to parse result value for " ++
                     genMethodStr True g ++ ": " ++ show v ++ " : " ++ msg
         ShellResult gss a -> do
                 sequence_ [ withStyle sty txt
                           | gs <- gss
                           , Glyph txt sty <- gs
                           ]
                 putStrLn "\n[Done]\n"
                 return a
send (Fail str) = fail str

genMethodStr :: Bool -> Value -> String
genMethodStr fl (Object o) =
    let mthd = fromJust $ parseMaybe (\ x -> x .: "method") o :: Text
        prms = fromJust $ parseMaybe (\ x -> x .: "params") o :: [Value]
        prms' = map (genMethodStr False) (reverse prms)
        wrap :: String -> String
        wrap str = if fl then " (" ++ str ++ ")" else " " ++ str in
      unpack mthd ++ wrap (unwords prms')
genMethodStr _ (Array vec) =
    show . map (genMethodStr False) $ toList vec
genMethodStr _ (String str) = unpack str
genMethodStr _ (Number n) = show n
genMethodStr _ (Bool b) = show b
genMethodStr _ Null = ""

