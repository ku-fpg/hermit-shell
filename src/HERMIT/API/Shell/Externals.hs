{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Shell.Externals where

import HERMIT.API.Types

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

-- XXX: Fix me
--up :: KernelEffect
--up = KernelEffect $ method "up" []



