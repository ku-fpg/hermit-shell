{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API.Shell where
        
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe

import HERMIT.API.Types

-- | redisplays current state.
display :: Shell ()
display = Shell $ method "display" []

-- | stops HERMIT; resumes compile.
resume :: Shell ()
resume = Shell $ method "resume" []

-- | promote a `Transform` to top-level, run it, and print the result.
query :: (Guts a) => Transform a b -> Shell ()
query (Transform t) = Shell $ method "query" [t]
        
-- | promote a `Rewrite` to top-level, run it, and update global state with the result.
rewrite :: Guts a => Rewrite a -> Shell ()
rewrite (Transform t) = Shell $ method "rewrite" [t]

-- | set the local path, based on a transformation.
setPath :: Guts a => Transform a LocalPath -> Shell ()
setPath (Transform t) = Shell $ method "setPath" [t]
