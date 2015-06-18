{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API
script = do
  setPath (bindingOf "fib")
  query (remember "orig-fib")
  apply (anyCall (unfoldRemembered "orig-fib"))
  resume
  
