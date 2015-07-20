{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.KernelEffect where

import           HERMIT.Server.Parser.Utils

import           HERMIT.Shell.KernelEffect
import           HERMIT.Shell.Types

instance External KernelEffect where
  parseExternals =
    [ external "up" (Direction U)
    , external "top" (Direction T)
    , external "beginScope" BeginScope
    , external "endScope"    EndScope
    ]

