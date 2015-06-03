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

diff :: AST -> AST -> ShellEffect
diff a b = ShellEffect $ method "diff" [Number (fromInteger $ toInteger a), Number (fromInteger $ toInteger b)]

