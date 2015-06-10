{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API

import WWAssAScript

script :: Shell ()
script = do
  query flattenModule
  query $ bindingOf "fac"
  apply $ wwSplit "wrap" "unwrap" (wwAssAToAssC wwa)
  eval "bash-extended-with [ case-elim-inline-scrutinee, inline [ \"unwrap\", \"wrap\", \"*\", \"-\" ] ]"
  eval "{"
  eval "defRhs"
  eval "letBody"
  eval "alpha-lam 'n"
  eval "}"

