{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
#include "overlap.h"
__LANGUAGE_OVERLAPPING_INSTANCES__
module HERMIT.API.Shell where

import Data.Aeson

import HERMIT.API.Types
import HERMIT.API.Shell.Externals (beginScope, endScope)

-- | redisplays current state.
display :: Shell ()
display = Shell $ method "display" []

-- | stops HERMIT; resumes compile.
resume :: Shell ()
resume = Shell $ method "resume" []

-- | promote a `Transform` to top-level, run it, and print the result.
query :: Guts a => Transform a b -> Shell ()
query (Transform t) = Shell $ method "query" [t]

-- | promote a `Rewrite` to top-level, run it, and update global state with the result.
--   (We share the same command name as is Isabelle)
apply :: Guts a => Rewrite a -> Shell ()
apply (Transform t) = Shell $ method "rewrite" [t]

-- | set the local path, based on a transformation.
setPath :: Guts a => Transform a LocalPath -> Shell ()
setPath (Transform t) = Shell $ method "setPath" [t]


-- TODO: Make sure this is the right way to do it.
sendCrumb :: Crumb -> Shell ()
sendCrumb (Crumb c) = Shell $ method "setPath" [c]

-- | backdoor into the old shell. This will be removed at some point.
eval :: String -> Shell ()
eval s = Shell $ method "eval" [toJSON s]

-- TODO: Add JSON wrapping to this
kernelEffect :: KernelEffect -> Shell ()
kernelEffect (KernelEffect e) = Shell $ toJSON e

shellEffect :: ShellEffect -> Shell ()
shellEffect (ShellEffect e) = Shell $ toJSON e

-- | Lift a normal command into a user proof command
toProofCmd :: Transform LCoreTC () -> Shell ()
toProofCmd (Transform t) = Shell $ toJSON t

proofCmd :: ProofShellCommand -> Shell ()
proofCmd (ProofShellCommand c) = Shell c

-- | Brackets the given argument with 'kernelEffect beginScope'
--   and 'kernelEffect endScope'.
scope :: Shell () -> Shell ()
scope s = do
  kernelEffect beginScope
  s
  kernelEffect endScope

pathS :: [Crumb] -> Rewrite a -> Rewrite a
pathS crumbs r
  = Transform
  $ method "pathS"
           [ toJSON crumbs
           , toJSON r
           ]

class Run a where
  run :: a -> Shell ()
 
instance Run Crumb where
  run = sendCrumb
 
instance Guts a => Run (Transform a LocalPath) where
  run = setPath
 
instance Guts a => Run (Transform a a) where
  run = apply
 
instance __OVERLAPPABLE__ Guts a => Run (Transform a b) where
  run = query

