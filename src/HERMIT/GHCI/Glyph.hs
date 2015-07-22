{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-} -- Will clean up
{-# LANGUAGE UndecidableInstances #-} -- TODO: clean up

module HERMIT.GHCI.Glyph where

import           Data.Aeson

import           System.Console.ANSI
        
-- | Glyphs
newtype Glyphs = Glyphs [Glyph]

deriving instance ToJSON Glyph   => ToJSON Glyphs
deriving instance FromJSON Glyph => FromJSON Glyphs

-- | Glyph
data Glyph = Glyph { gText :: String
                   , gStyle :: Maybe Style
                   } deriving Show

showGlyph :: Glyph -> String
showGlyph = gText

-- TODO: replace with SyntaxForColor
-- | Style
data Style = KEYWORD | SYNTAX | VAR | COERCION | TYPE | LIT | WARNING
    deriving (Eq, Read, Show)

withStyle :: Maybe Style -> String -> IO ()
withStyle Nothing    str = putStr str
withStyle (Just sty) str = do
  setSGR $ styleSGR sty
  putStr str
  setSGR [Reset]

withNoStyle :: Maybe Style -> String -> IO ()
withNoStyle _ str = putStr str

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

