module LastScript where
import HERMIT.API

import WWAssBScript

script :: Shell ()
script = do
  apply flattenModule
  setPath $ bindingOf "last"
  apply $ wwSplitStaticArg 1 [0] "wrap" "unwrap" (wwAssBToAssC wwb)
  apply $ bashExtendedWith [ inlineAny [ "f", "wrap", "unwrap" ] ]
  apply unshadow

