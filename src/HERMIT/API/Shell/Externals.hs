{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Shell.Externals where

import           HERMIT.API.Types

import           Data.Aeson

-- | stops HERMIT; resumes compile
--resume :: ShellEffect
--resume = ShellEffect $ method

-- | hard UNIX-style exit; does not return to GHC; does not save
abort :: ShellEffect
abort = ShellEffect $ method "abort" []

-- | exits shell; resumes HERMIT
continue :: ShellEffect
continue = ShellEffect $ method "continue" []

-- | garbage-collect all ASTs except for the initial and current AST
-- gc :: ShellEffect

-- | garbage-collect a given AST
-- gc :: ShellEffect

-- | move to the parent node
up :: KernelEffect
up = KernelEffect $ method "up" []

-- | switch to navigate mode
--navigate :: KernelEffect
--navigate = KernelEffect $ method "navigate" []

-- | switch to command line mode
--commandLine :: ShellEffect
--commandLine = ShellEffect $ method "commandLine" []

-- | fix the window to the current focus
setWindow :: ShellEffect
setWindow = ShellEffect $ method "setWindow" []

-- | move to root of current scope
top :: KernelEffect
top = KernelEffect $ method "top" []

-- | go back in the derivation
log :: QueryFun
log = QueryFun $ method "log" []

-- | go back in the derivation
back :: ShellEffect
back = ShellEffect $ method "back" []

-- | step forward in the derivation
step :: ShellEffect
step = ShellEffect $ method "step" []

-- | goto a specific step in the derivation
--goto :: AST -> ShellEffect

-- | goto a specific step in the derivation by tag name
--goto :: String -> ShellEffect

-- | name the current step in the derivation
tag :: String -> ShellEffect
tag name = ShellEffect $ method "tag" [toJSON name]

-- | show diff of two ASTs
diff :: AST -> AST -> QueryFun
diff a b = QueryFun $ method "diff" [toJSON a, toJSON b]

-- |  print diffs rather than full code after a rewrite
setPPDiffOnly :: Bool -> ShellEffect
setPPDiffOnly b = ShellEffect $ method "setPPDiffOnly" [toJSON $ show b]

-- | any rewrite failure causes compilation to abort
setFailHard :: Bool -> ShellEffect
setFailHard b = ShellEffect $ method "setFailHard" [toJSON $ show b]

-- | run core lint type-checker after every rewrite, reverting on failure
setAutoCorelint :: Bool -> ShellEffect
setAutoCorelint b = ShellEffect $ method "setAutoCorelint" [toJSON $ show b]

-- | set the pretty printer
--   use 'setPP ls' to list available pretty printers"
setPP :: String -> ShellEffect
setPP s = ShellEffect $ method "setPP" [toJSON s]

-- setPPRenderer

-- dump

-- | Dump named lemma to a file.
--   "dump-lemma <pretty-printer> <lemma-name> <filename> <renderer> <width>
dumpLemma :: PrettyPrinter -> LemmaName -> FilePath -> String -> Int -> Transform LCoreTC ()
dumpLemma pp nm fp r w
  = Transform
  $ method "dumpLemma"
           [ toJSON pp
           , toJSON nm
           , toJSON fp
           , toJSON r
           , toJSON w
           ]

-- | set the width of the screen
setPPWidth :: Int -> ShellEffect
setPPWidth width = ShellEffect $ method "setPPWidth" [toJSON width]

-- | set how to show expression-level types (Show|Abstact|Omit)
setPPType :: PPType -> ShellEffect
setPPType ppType = ShellEffect $ method "setPPType" [toJSON $ show ppType]

-- | set how to show coercions (Show|Abstact|Omit)
setPPCoercion :: PPType -> ShellEffect
setPPCoercion ppType = ShellEffect $ method "setPPCoercion" [toJSON $ show ppType]

-- | set whether uniques are printed with variable names
setPPUniques :: Bool -> ShellEffect
setPPUniques b = ShellEffect $ method "setPPUniques" [toJSON $ show b]

-- | push current lens onto a stack
beginScope :: KernelEffect
beginScope = KernelEffect $ method "beginScope" []

-- | pop a lens off a stack
endScope :: KernelEffect
endScope = KernelEffect $ method "endScope" []

-- | load <script-name> <file-name> : load a HERMIT script from a file and save it under the specified name.
load :: String -> String -> ScriptEffect
load scriptName fileName = ScriptEffect $ method "load" [toJSON scriptName, toJSON fileName]

-- | loadAndRun <file-name> : load a HERMIT script from a file and run it immediately.
loadAndRun :: String -> ScriptEffect
loadAndRun fileName = ScriptEffect $ method "loadAndRun" [toJSON fileName]

-- | save <filename> : save the current complete derivation into a file.
save :: String -> ScriptEffect
save fileName = ScriptEffect $ method "save" [toJSON fileName]

-- | saveVerbose <filename> : save the current complete derivation into a file,
--   including output of each command as a comment.
saveVerbose :: String -> ScriptEffect
saveVerbose fileName = ScriptEffect $ method "saveVerbose" [toJSON fileName]

-- | save-script <filename> <script name> : save a loaded or manually defined script to a file.
saveScript :: String -> String -> ScriptEffect
saveScript fileName scriptName = ScriptEffect $ method "saveScript" [toJSON fileName, toJSON scriptName]

-- | loadAsRewrite <rewrite-name> <filepath> : load a HERMIT script from a file, and convert it to a rewrite.
--   Note that there are significant limitations on the commands the script may contain.
loadAsRewrite :: String -> String -> ScriptEffect
loadAsRewrite rewriteName filePath = ScriptEffect $ method "loadAsRewrite" [toJSON rewriteName, toJSON filePath]

-- |  scriptToRewrite <rewrite-name> <script-name> : create a new rewrite from a pre-loaded (or manually defined) HERMIT script.
--    Note that there are significant limitations on the commands the script may contain.
scriptToRewrite :: String -> String -> ScriptEffect
scriptToRewrite rewriteName filePath = ScriptEffect $ method "scriptToRewrite" [toJSON rewriteName, toJSON filePath]

-- |  Define a new HERMIT script and bind it to a name.
--    Note that any names in the script will not be resolved until the script is *run*.
--    Example usage: defineScript "MyScriptName" "anyTd betaReduce ; letSubst ; bash"
defineScript :: String -> String -> ScriptEffect
defineScript scriptName script = ScriptEffect $ method "defineScript" [toJSON scriptName, toJSON script]

-- |  Define a new HERMIT rewrite and bind it to a name.
--    Note that this also saves the input script under the same name.
--    Example usage: defineRewrite "MyRewriteName" "let-subst >>> bash"
defineRewrite :: String -> String -> ScriptEffect
defineRewrite rewriteName rewrite = ScriptEffect $ method "defineRewrite" [toJSON rewriteName, toJSON rewrite]

-- |  Run a pre-loaded (or manually defined) HERMIT script.
--    Note that any names in the script will not be resolved until the script is *run*.
runScript :: String -> ScriptEffect
runScript script = ScriptEffect $ method "runScript" [toJSON script]

-- | Display all loaded scripts.
displayScripts :: QueryFun
displayScripts = QueryFun $ method "displayScripts" []

-- | Stop running the current script.
stopScript :: ShellEffect
stopScript = ShellEffect $ method "stopScript" []

