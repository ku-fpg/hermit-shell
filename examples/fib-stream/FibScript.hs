{-# LANGUAGE OverloadedStrings #-}
module FibScript (script) where

import HERMIT.API

script :: Shell ()
script = do
  apply flattenModule
  setPath $ bindingOf "fib"

  scope $ do
    apply $ wwSplitUnsafe "wrap'" "unwrap'"
    scope $ do
      setPath $ rhsOf "work"
      apply . anyCall $ unfoldWith "f"
      scope $ do
        setPath $ consider cLam ; apply $ alphaLamWith "m"
    apply simplify
    apply . anyCall $ unfoldWith "wrap'"

