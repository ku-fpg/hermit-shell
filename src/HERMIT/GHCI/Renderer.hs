{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.GHCI.Renderer where

import Control.Concurrent.STM

import HERMIT.Core
import HERMIT.Kure
import HERMIT.PrettyPrinter.Common
import HERMIT.PrettyPrinter.Glyphs

import HERMIT.API.Types
import HERMIT.GHCI.Types

import Prelude.Compat

import Data.Aeson
    
import System.Console.ANSI
import System.IO

instance Response Glyphs where
  printResponse (Glyphs gs) =
         do sequence_ [ withStyle sty txt
                      | Glyph txt sty <- gs
                      ]
            putStr "\n"

webChannel :: TChan [Glyph] -> Handle -> PrettyOptions -> Either String DocH -> IO ()
webChannel chan _ _    (Left s)    = atomically $ writeTChan chan [Glyph s Nothing]
webChannel chan _ opts (Right doc) = let Glyphs gs = renderCode opts doc
                                     in atomically $ writeTChan chan gs
