{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API
script = do
  setPath (bindingOf "fib")
  query (remember "orig-fib")
  rewrite (anyCall (unfoldRemembered "orig-fib"))
  resume
  
