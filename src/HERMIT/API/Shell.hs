{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API.Shell where

import HERMIT.API.Types

-- | redisplays current state.
display :: Shell ()
display = Shell $ method "display" []

-- | stops HERMIT; resumes compile.
resume :: Shell ()
resume = Shell $ method "resume" []

-- | promote a `Transform` to top-level, run it, and return the result.
query :: (Guts a, Response b) => Transform a b -> Shell b
query (Transform t) = Shell $ method "query" [t]
        
-- | promote a `Rewrite` to top-level, run it, and update global state with the result.
rewrite :: Guts a => Rewrite a -> Shell ()
rewrite (Transform t) = Shell $ method "rewrite" [t]

-- | set the local path, 
setPath :: Guts a => Transform a LocalPath -> Shell ()
setPath (Transform t) = Shell $ method "setPath" [t]
