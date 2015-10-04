{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
#include "overlap.h"
__LANGUAGE_OVERLAPPING_INSTANCES__
module HERMIT.API.Shell where

import Data.Aeson

import HERMIT.API.Types
import HERMIT.API.Shell.Externals (beginScope, endScope)
import HERMIT.API.Shell.Proof (proveLemma, endProof)
import HERMIT.API.Dictionary.KURE (serialise)

import HERMIT.RemoteShell.Types

-- | redisplays current state.
display :: Shell ()
display = Shell $ method "display" []

-- | Returns the current state.
display' :: Shell Document
display' = Shell $ method "display$" []

-- | stops HERMIT; resumes compile.
resume :: Shell ()
resume = Shell $ method "resume" []

-- | promote a `Transform` to top-level, run it, and print the result.
query :: (Guts a, FromJSON b) => Transform a b -> Shell b
query (Transform t) = Shell $ method "query" [t]

queryFun :: QueryFun -> Shell ()
queryFun (QueryFun q) = Shell $ method "query" [q]

-- | promote a `Rewrite` to top-level, run it, and update global state with the result.
--   (We share the same command name as is Isabelle)
apply :: Guts a => Rewrite a -> Shell ()
apply (Transform t) = Shell $ method "rewrite" [t]

-- | set the local path, based on a transformation.
setPath :: Guts a => Transform a LocalPath -> Shell ()
setPath (Transform t) = Shell $ method "setPath" [t]


-- TODO: Make sure this is the right way to do it.
-- TODO: this is *not* composable.
sendCrumb :: Crumb -> Shell ()
sendCrumb (Crumb c) = Shell $ method "setPath" [c]

-- | backdoor into the old shell. This will be removed at some point.
eval :: String -> Shell ()
eval s = Shell $ method "eval" [toJSON s]

-- TODO: Add JSON wrapping to this
kernelEffect :: KernelEffect -> Shell ()
kernelEffect (KernelEffect e) = Shell $ toJSON e

shellEffect :: FromJSON a => ShellEffect a -> Shell a
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

-- TODO: See if we should implement a Proof monad and use it here.
proof :: LemmaName -> Shell () -> Shell ()
proof lemmaName p = do
  shellEffect $ proveLemma lemmaName
  p
  proofCmd endProof

pathR :: [Crumb] -> Rewrite a -> Rewrite a
pathR crumbs r
  = Transform
  $ method "pathR"
           [ toJSON crumbs
           , toJSON r
           ]

pathRs :: [Crumb] -> [Rewrite a] -> Rewrite a
pathRs crumbs = pathR crumbs . serialise

-- TODO: See if this can implemented in terms of pathR
pathS :: [Crumb] -> Shell () -> Shell ()
pathS crumbs s =
  scope $ do
    mapM_ sendCrumb crumbs
    s

class Run a where
  run :: a -> Shell ()

instance Run QueryFun where
  run = queryFun

instance Run Crumb where
  run = sendCrumb

instance Guts a => Run (Transform a LocalPath) where
  run = setPath

instance Guts a => Run (Transform a a) where
  run = apply

{-
instance __OVERLAPPABLE__ Guts a => Run (Transform a b) where
  run = query
-}
