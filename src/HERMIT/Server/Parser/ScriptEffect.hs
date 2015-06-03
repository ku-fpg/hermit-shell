{-# LANGUAGE OverloadedStrings #-}
module HERMIT.Server.Parser.ScriptEffect where

import           HERMIT.Shell.ScriptToRewrite
import           HERMIT.Server.Parser.Utils

instance External ScriptEffect where
  parseExternals =
    [ external "load" LoadFile
        ["load <script-name> <file-name> : load a HERMIT script from a file and save it under the specified name."]
    , external "loadAndRun"  loadAndRun
        ["loadAndRun <file-name> : load a HERMIT script from a file and run it immediately."]
    ]

