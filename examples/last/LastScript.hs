{-# LANGUAGE NoImplicitPrelude #-}
module LastScript where

import HERMIT.API.Prelude

import WWAssBScript

script :: Shell ()
script = do
  apply flattenModule
  setPath $ bindingOf "last"
  apply $ wwSplitStaticArg 1 [0] "wrap" "unwrap" (wwAssBToAssC wwb)
  apply $ bashExtendedWith [ inlineWith [ "f", "wrap", "unwrap" ] ]
  apply unshadow

