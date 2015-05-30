{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API
script = do
  -- eval "binding-of 'fib"
  setPath (bindingOf "fib")
  query (remember "orig-fib")
  apply (anyCall (unfoldRemembered "orig-fib"))
  resume
  
