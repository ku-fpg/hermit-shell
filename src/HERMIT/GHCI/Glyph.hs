{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module HERMIT.GHCI.Glyph where

import           Control.Monad (mzero)

import           Data.Aeson

import           HERMIT.Kernel (AST)
import           HERMIT.GHCI.JSON

import           System.Console.ANSI

-- | Glyphs
newtype Glyphs = Glyphs [Glyph]
  deriving (FromJSON, ToJSON)

-- | Glyph
data Glyph = Glyph { gText :: String
                   , gStyle :: Maybe Style
                   } deriving Show

instance ToJSON Glyph where
    toJSON g = object $ ("text" .= gText g) : fromMaybeAttr "style" (gStyle g)

instance FromJSON Glyph where
    parseJSON (Object v) = Glyph <$> v .: "text"
                                 <*> v .:? "style"
    parseJSON _          = mzero

-- | CommandResponse
data CommandResponse = CommandResponse { crMsg :: Maybe String
                                       , crGlyphs :: Maybe [Glyph]
                                       , crAst :: AST
                                       }

instance ToJSON CommandResponse where
    toJSON cr = object $ ("ast" .= crAst cr) : fromMaybeAttr "glyphs" (crGlyphs cr) ++ fromMaybeAttr "msg" (crMsg cr)

instance FromJSON CommandResponse where
    parseJSON (Object v) = CommandResponse <$> v .:? "msg" <*> v .:? "glyphs" <*> v .: "ast"
    parseJSON _          = mzero

showGlyph :: Glyph -> String
showGlyph = gText

-- TODO: replace with SyntaxForColor
-- | Style
data Style = KEYWORD | SYNTAX | VAR | COERCION | TYPE | LIT | WARNING
    deriving (Eq, Read, Show)

instance ToJSON Style where
    toJSON = stringToJSON

instance FromJSON Style where
    parseJSON = fromJSONString

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

