{-# LANGUAGE NoImplicitPrelude #-}
module FibScript where

import HERMIT.API.Prelude

script :: Shell ()
script = do
  setPath (bindingOf "fib")
  query (remember "orig-fib")
  apply (anyCall (unfoldRemembered "orig-fib"))
  resume

