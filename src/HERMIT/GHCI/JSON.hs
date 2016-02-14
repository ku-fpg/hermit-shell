{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.GHCI.JSON where

import           Control.Monad

import           Data.Aeson hiding (json)
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           HERMIT.External
import           HERMIT.Kernel (AST)

import           System.Console.Haskeline.Completion (Completion(..))

import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.ByteString.Lazy.Char8 (unpack)

type UserID = Integer

-- | Msg
data Msg = Msg { mMsg :: String }

instance ToJSON Msg where
    toJSON (Msg m) = object [ "msg" .= m ]

instance FromJSON Msg where
    parseJSON (Object v) = Msg <$> v .: "msg"
    parseJSON _          = mzero

-- | Token
data Token = Token { tUser :: Integer , tAst :: AST }
  deriving Show

instance ToJSON Token where
    toJSON (Token u a) = object [ "user" .= u , "ast" .= a ]

instance FromJSON Token where
    parseJSON (Object v) = Token <$> v .: "user" <*> v .: "ast"
    parseJSON _          = mzero

-- | Command
data Command = Command { cToken :: Token
                       , cCmd :: String
                       , cWidth :: Maybe Int
                       }

instance ToJSON Command where
    toJSON cmd = object $ fromMaybeAttr "width" (cWidth cmd) ++ [ "token" .= cToken cmd , "cmd" .= cCmd cmd ]

instance FromJSON Command where
    parseJSON (Object v) = Command <$> v .: "token" <*> v .: "cmd" <*> v .:? "width"
    parseJSON _          = mzero

-- | AST
instance ToJSON AST where
    toJSON = toJSON . fromEnum

instance FromJSON AST where
    parseJSON j = toEnum <$> parseJSON j

fromMaybeAttr :: ToJSON a => T.Text -> Maybe a -> [Pair]
fromMaybeAttr nm = maybe [] (\ a -> [nm .= a])

-- | CommandList
data CommandList = CommandList { clCmds :: [CommandInfo] }

instance ToJSON CommandList where
    toJSON cl = object [ "cmds" .= clCmds cl ]

instance FromJSON CommandList where
    parseJSON (Object v) = CommandList <$> v .: "cmds"
    parseJSON _          = mzero

-- | CommandInfo
data CommandInfo = CommandInfo { ciName :: String
                               , ciHelp :: String
                               , ciTags :: [CmdTag]
                               , ciArgTys :: [String]
                               , ciResTy :: String
                               }

instance ToJSON CommandInfo where
    toJSON ci = object [ "name" .= ciName ci , "help" .= ciHelp ci , "tags" .= ciTags ci, "argTys" .= ciArgTys ci, "resTy" .= ciResTy ci ]

instance FromJSON CommandInfo where
    parseJSON (Object v) = CommandInfo <$> v .: "name" <*> v .: "help" <*> v .: "tags" <*> v .: "argTys" <*> v .: "resTy"
    parseJSON _          = mzero

-- | CmdTag
instance ToJSON CmdTag where
    toJSON = stringToJSON

instance FromJSON CmdTag where
    parseJSON = fromJSONString

stringToJSON :: Show a => a -> Value
stringToJSON = String . T.pack . show

fromJSONString :: Read a => Value -> Parser a
fromJSONString (String s) =
    case readEither $ TL.fromStrict s of
        Left _msg -> mzero
        Right sty -> pure sty
fromJSONString _ = mzero

-- from Web.Scotty.Action
readEither :: Read a => TL.Text -> Either TL.Text a
readEither t = case [ x | (x,"") <- reads (TL.unpack t) ] of
                [x] -> Right x
                []  -> Left "readEither: no parse"
                _   -> Left "readEither: ambiguous parse"


data History = History { hCmds :: [HCmd]
                       , hTags :: [HTag]
                       }

data HCmd = HCmd AST String AST
data HTag = HTag String AST

instance ToJSON History where
    toJSON h = object [ "cmds" .= hCmds h , "tags" .= hTags h ]

instance FromJSON History where
    parseJSON (Object v) = History <$> v .: "cmds" <*> v .: "tags"
    parseJSON _          = mzero

instance ToJSON HCmd where
    toJSON (HCmd from e to) = object [ "from" .= from , "cmd" .= e , "to" .= to ]

instance FromJSON HCmd where
    parseJSON (Object v) = HCmd <$> v .: "from" <*> v .: "cmd" <*> v .: "to"
    parseJSON _          = mzero

instance ToJSON HTag where
    toJSON (HTag tag ast) = object [ "tag" .= tag , "ast" .= ast ]

instance FromJSON HTag where
    parseJSON (Object v) = HTag <$> v .: "tag" <*> v .: "ast"
    parseJSON _          = mzero


-- The Show instance for Value prints out Vector literals, which have
-- different output depending on which version of vector is being used.
-- This is inconvenient for diffing purposes, so we use a pretty-printer
-- to make the output more consistent.
pprintJSON :: Value -> String
pprintJSON = unpack . encodePretty
