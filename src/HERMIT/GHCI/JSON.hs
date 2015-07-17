{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.GHCI.JSON where

import           Control.Monad

import           Data.Aeson hiding (json)
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           HERMIT.Core (Crumb(..))
import           HERMIT.External
import           HERMIT.Kernel (AST)

import           Prelude.Compat

import           System.Console.Haskeline.Completion (Completion(..))

import           Web.Scotty (readEither)

import           HERMIT.GHCI.Glyph


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


-- | CommandResponse
data CommandResponse = CommandResponse { crMsg :: Maybe String
                                       , crGlyphs :: Maybe [Glyph]
                                       , crAst :: AST
                                       }

fromMaybeAttr :: ToJSON a => T.Text -> Maybe a -> [Pair]
fromMaybeAttr nm = maybe [] (\ a -> [nm .= a])

instance ToJSON CommandResponse where
    toJSON cr = object $ ("ast" .= crAst cr) : fromMaybeAttr "glyphs" (crGlyphs cr) ++ fromMaybeAttr "msg" (crMsg cr)

instance FromJSON CommandResponse where
    parseJSON (Object v) = CommandResponse <$> v .:? "msg" <*> v .:? "glyphs" <*> v .: "ast"
    parseJSON _          = mzero

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

instance ToJSON Style where
    toJSON = stringToJSON

instance FromJSON Style where
    parseJSON = fromJSONString

stringToJSON :: Show a => a -> Value
stringToJSON = String . T.pack . show

fromJSONString :: Read a => Value -> Parser a
fromJSONString (String s) =
    case readEither $ TL.fromStrict s of
        Left _msg -> mzero
        Right sty -> pure sty
fromJSONString _ = mzero


instance ToJSON Glyph where
    toJSON g = object $ ("text" .= gText g) : fromMaybeAttr "style" (gStyle g)

instance FromJSON Glyph where
    parseJSON (Object v) = Glyph <$> v .: "text"
                                 <*> v .:? "style"
    parseJSON _          = mzero

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

data Complete = Complete { cpUser :: UserID
                         , cpCmd :: String
                         }

instance ToJSON Complete where
    toJSON c = object [ "user" .= cpUser c , "cmd" .= cpCmd c ]

instance FromJSON Complete where
    parseJSON (Object v) = Complete <$> v .: "user" <*> v .: "cmd"
    parseJSON _          = mzero

data Completions = Completions { cCompletions :: [Completion] }

instance ToJSON Completions where
    toJSON (Completions cs) = object [ "completions" .= cs ]

instance FromJSON Completions where
    parseJSON (Object v) = Completions <$> v .: "cs"
    parseJSON _          = mzero

instance ToJSON Completion where
    toJSON c = object [ "replacement" .= replacement c , "display" .= display c , "isFinished" .= isFinished c ]

instance FromJSON Completion where
    parseJSON (Object v) = Completion <$> v .: "replacement" <*> v .: "display" <*> v .: "isFinished"
    parseJSON _          = mzero

data Transport :: (* -> *) -> * where
  Transport :: (ToJSON (f a), ToJSON a) => f a -> Transport f

instance ToJSON (Transport f) where
  toJSON (Transport f) = toJSON f


data HermitCommand :: * -> * -> * where
   BetaReduce :: HermitCommand () ()
--   AnyTD      :: HermitCommand () -> HermitCommand ()
--   ShellEffect :: ShellEffect ->
   Display    :: HermitCommand () ()

--
