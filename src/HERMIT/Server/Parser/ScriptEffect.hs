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
    , external "save" (SaveFile False)
        ["save <filename> : save the current complete derivation into a file."]
    , external "saveVerbose" (SaveFile True)
        ["save-verbose <filename> : save the current complete derivation into a file,"
        ,"including output of each command as a comment."
        ]
    , external "saveScript"  SaveScript
        ["save-script <filename> <script name> : save a loaded or manually defined script to a file." ]
    , external "loadAsRewrite" (\ rewriteName fileName -> SeqMeta [LoadFile rewriteName fileName, ScriptToRewrite rewriteName rewriteName])
        ["loadAsRewrite <rewrite-name> <filepath> : load a HERMIT script from a file, and convert it to a rewrite."
        ,"Note that there are significant limitations on the commands the script may contain."] .+ Experiment .+ TODO
    ]

