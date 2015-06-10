{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Shell where

import Data.Aeson

import HERMIT.API.Types

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
