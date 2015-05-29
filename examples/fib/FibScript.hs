{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API
script = do
  -- eval "binding-of 'fib"
  setPath (bindingOf "fib")
  query (remember "orig-fib")
  rewrite (anyCall (unfoldRemembered "orig-fib"))
  resume
  
