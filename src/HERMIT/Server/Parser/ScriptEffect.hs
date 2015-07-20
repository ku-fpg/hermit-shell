{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.ScriptEffect where

import           HERMIT.Shell.ScriptToRewrite
import           HERMIT.Server.Parser.Utils

instance External ScriptEffect where
  parseExternals =
    [ external "load" LoadFile
    , external "loadAndRun"  loadAndRun
    , external "save" (SaveFile False)
    , external "saveVerbose" (SaveFile True)
    , external "saveScript"  SaveScript
    , external "loadAsRewrite" (\ rewriteName fileName -> SeqMeta [LoadFile rewriteName fileName, ScriptToRewrite rewriteName rewriteName])
    , external "scriptToRewrite" ScriptToRewrite
    , external "defineScript" DefineScript
    , external "defineRewrite" (\ name str -> SeqMeta [DefineScript name str, ScriptToRewrite name name])
    , external "runScript"  RunScript
    ]

