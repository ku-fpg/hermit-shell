{-# LANGUAGE NoImplicitPrelude #-}
module FibScript (script) where

import HERMIT.API.Prelude

script :: Shell ()
script = do
  apply flattenModule
  setPath $ bindingOf "fib"

  scope $ do
    apply $ wwSplitUnsafe "wrap Nat" "unwrap Nat"
    scope $ do
      setPath $ rhsOf "work"
      apply . anyCall $ unfoldWith "f"
      scope $ do
        setPath $ consider cLam ; apply $ alphaLamWith "m"
    apply simplify
    apply . anyCall $ unfoldWith "wrap"
