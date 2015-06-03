{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Shell.Externals where

import           HERMIT.API.Types

import           Data.Aeson
import           Data.String (fromString)

-- | stops HERMIT; resumes compile
-- resume :: ShellEffect

-- | hard UNIX-style exit; does not return to GHC; does not save
-- abort :: ShellEffect

-- | exits shell; resumes HERMIT
-- continue :: ShellEffect

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
tag name = ShellEffect $ method "tag" [String (fromString name)]

-- | show diff of two ASTs
diff :: AST -> AST -> QueryFun
diff a b = QueryFun $ method "diff" [Number . fromInteger $ toInteger a, Number . fromInteger $ toInteger b]

-- |  print diffs rather than full code after a rewrite
setPpDiffOnly :: Bool -> ShellEffect
setPpDiffOnly b = ShellEffect $ method "setPpDiffOnly" [String . fromString $ show b]

-- | any rewrite failure causes compilation to abort
setFailHard :: Bool -> ShellEffect
setFailHard b = ShellEffect $ method "setFailHard" [String . fromString $ show b]

-- | run core lint type-checker after every rewrite, reverting on failure
setAutoCorelint :: Bool -> ShellEffect
setAutoCorelint b = ShellEffect $ method "setAutoCorelint" [String . fromString $ show b]

-- | set the pretty printer
--   use 'setPp ls' to list available pretty printers"
setPp :: String -> ShellEffect
setPp s = ShellEffect $ method "setPp" [String (fromString s)]

-- setPpRenderer

-- dump

-- dump-lemma

-- | set the width of the screen
setPpWidth :: Int -> ShellEffect
setPpWidth width = ShellEffect $ method "setPpWidth" [Number . fromInteger $ toInteger width]

-- | set how to show expression-level types (Show|Abstact|Omit)
setPpType :: PpType -> ShellEffect
setPpType ppType = ShellEffect $ method "setPpType" [String . fromString $ show ppType]

-- | set how to show coercions (Show|Abstact|Omit)
setPpCoercion :: PpType -> ShellEffect
setPpCoercion ppType = ShellEffect $ method "setPpCoercion" [String . fromString $ show ppType]

-- | set whether uniques are printed with variable names
setPpUniques :: Bool -> ShellEffect
setPpUniques b = ShellEffect $ method "setPpUniques" [String . fromString $ show b]

-- | push current lens onto a stack
beginScope :: KernelEffect
beginScope = KernelEffect $ method "beginScope" []

-- | pop a lens off a stack
endScope :: KernelEffect
endScope = KernelEffect $ method "endScope" []

-- | load <script-name> <file-name> : load a HERMIT script from a file and save it under the specified name.
load :: String -> String -> ScriptEffect
load scriptName fileName = ScriptEffect $ method "load" [String $ fromString scriptName, String $ fromString fileName]

-- | loadAndRun <file-name> : load a HERMIT script from a file and run it immediately.
loadAndRun :: String -> ScriptEffect
loadAndRun fileName = ScriptEffect $ method "loadAndRun" [String $ fromString fileName]

