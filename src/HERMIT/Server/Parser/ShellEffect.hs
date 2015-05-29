{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.ShellEffect where

import           HERMIT.Shell.ShellEffect
import           HERMIT.Shell.Types

import           HERMIT.Server.Parser.Utils

instance External ShellEffect where
  parseExternal = alts 
    [ external "resume"  Resume
        [ "stops HERMIT; resumes compile" ]
    , external "display" (CLSModify $ showWindowAlways Nothing)
        [ "redisplays current state" ]
    ]
