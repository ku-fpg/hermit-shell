{-# LANGUAGE OverloadedStrings #-}
module HERMIT.Server.Parser.KernelEffect where

import           HERMIT.Server.Parser.Utils

import           HERMIT.Shell.KernelEffect
import           HERMIT.Shell.Types

instance External KernelEffect where
  parseExternals =
    [ external "up" (Direction U)
        [ "move to the parent node" ]
    , external "top" (Direction T)
        [ "move to root of current scope" ]
    , external "beginScope" BeginScope
        ["push current lens onto a stack"]       -- tag as internal
    , external "endScope"    EndScope
        ["pop a lens off a stack"]               -- tag as internal
    ]

