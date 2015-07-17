{-# LANGUAGE NoImplicitPrelude #-}
module NewLastScript where

import HERMIT.API.Prelude

script :: Shell ()
script = do
  apply flattenModule
  shellEffect $ setPPType Show

  setPath $ bindingOf "last"
  apply $ fixIntro

  scope $ do setPath $ applicationOf "fix"
             apply $ split1Beta "last" "wrap" "unwrap"

               -- prove the assumption
             apply $ lhsR (repeat (anyCall (unfoldAny [".", "wrap", "unwrap"])))

             apply $ bothR smash
             proofCmd endProof

             apply $ repeat (anyCall (unfoldAny ["g", "wrap", "unwrap", "fix"]))
             apply bash

