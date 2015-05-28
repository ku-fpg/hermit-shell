{-# LANGUAGE LambdaCase, OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API 
        ( -- Modules
          module HERMIT.API.Path
        , module HERMIT.API.ShellEffect
          -- Utilties
        , send
        ) where

import HERMIT.API.ShellEffect
import HERMIT.API.Path
import HERMIT.API.Types

import HERMIT.GHCI.Client

{-        
import HERMIT.GHCI.Session
        
-- | redisplays the current state to STDOUT.
display :: IO ()
display = shellEffect Display
        

-- Not exported, but useful

newtype ShellEffect :: * where
  ShellEffect :: Value -> ShellEffect a

display' :: ShellEffect
display' = ShellEffect $ prim (mkName "HERMIT.display") $ []

newtype TypedEffectH :: * -> * where
  TypedEffectH :: Value -> TypedEffectH a     

shellEffect :: ShellEffect -> TypedEffectH
shellEffect = print 'ShellEffectH

prim :: Name -> [Value] -> Value
prim = undefined

--- Main call-HERMIT function

class Shell f where
  toShell   :: f a -> Value
  fromShell :: f a -> Value -> ShellResult a

data ShellResult a
  = ShellResult [[Glyph]] a -- When was said, what was returned
  | ShellFailure String -- something went wrong
    deriving Show

instance FromJSON a => FromJSON (ShellResult a) where
  parseJSON (Object o) = ShellResult <$> o .: "output"
                                     <*> o .: "result"
                      <|> return (ShellFailure "malformed Object returned from Server")                                      
  parseJSON _ = return (ShellFailure "Object not returned from Server")
        
data ShellEffect :: * -> * where
  ShellEffect :: Value -> ShellEffect ()

instance Shell ShellEffect where
  toShell (ShellEffect v) = v
  fromShell (ShellEffect {}) = fromJust . parseMaybe parseJSON

display :: ShellEffect ()
display = ShellEffect $ object ["method" .= ("display" :: String)]

session :: JSONRPC.Session
session = Session 
  { sync = \ v -> do
          r <- asJSON =<< post "http://localhost:3000/" (toJSON  v)
          return $ r ^. responseBody
  , async = \ v -> do
          post "http://localhost:3000/" (toJSON v)
          return ()
  }        

send :: Shell f => f a -> IO a
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
-}