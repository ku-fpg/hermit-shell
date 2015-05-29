{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.ShellEffect where

import           HERMIT.Shell.ShellEffect
import           HERMIT.Shell.Types

import           HERMIT.Server.Parser.Utils

instance External ShellEffect where
  parseExternal = alts 
    [ external "display" $ CLSModify $ showWindowAlways Nothing
    , external "resume"  $ Resume
    ]
