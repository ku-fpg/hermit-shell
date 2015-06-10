{-# LANGUAGE OverloadedStrings #-}
module WWAssAScript where

import HERMIT.API

--wwa :: Rewrite LCore
wwa = do
  eval "bash-extended-with [ inline [\"wrap\", \"unwrap\"] ]"
  query $ anyBU caseElimMergeAlts
  apply etaReduce

