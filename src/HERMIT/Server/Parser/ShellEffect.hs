{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.ShellEffect where

import           HERMIT.Shell.ShellEffect
import           HERMIT.Shell.Types

import           HERMIT.Server.Parser.Utils

import           Control.Monad.State (modify)
import           HERMIT.Shell.Externals


instance External ShellEffect where
  parseExternals =
    [ external "resume"  Resume
        [ "stops HERMIT; resumes compile" ]
    , external "display" (CLSModify $ showWindowAlways Nothing)
        [ "redisplays current state" ]
--  , external "navigate" (CLSModify $ modify $ \ st -> st { cl_nav = True })
--      [ "switch to navigate mode" ]
    , external "setWindow" (CLSModify $ setWindow >> showWindow Nothing)
        [ "fix the window to the current focus" ]
    , external "back"            (CLSModify $ versionCmd Back)
        [ "go back in the derivation" ]
    , external "step"            (CLSModify $ versionCmd Step)
        [ "step forward in the derivation" ]
    , external "tag"             (CLSModify . versionCmd . Tag)
        [ "name the current step in the derivation" ]
    ]

