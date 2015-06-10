{-# LANGUAGE OverloadedStrings #-}
module MeanScript where

import HERMIT.API

import Data.Aeson


script :: Shell ()
script = do
  eval "{"
  setPath $ rhsOf "mean"
  sendCrumb lamBody

  eval "{"
  setPath $ arg 2
  apply $ letIntro "s"
  eval "}"

  eval "{"
  setPath $ arg 3
  apply $ letIntro "l"
  eval "}"

  apply $ innermost letFloat
  apply . try $ reorderLets ["s", "l"]
  apply $ letTuple "sl"

  eval "{"
  sendCrumb caseExpr
  apply $ abstract "xs"
  sendCrumb appFun
  apply $ letIntro "sumlength"
  eval "}"

  eval "}"


  apply $ innermost letFloat
  setPath $ bindingGroupOf "sumlength"
  apply nonrecToRec
  setPath $ bindingOf "sumlength"
  query $ remember "sumlen"


  eval "{ [def-rhs, lam-body]"  -- Why are there square brackets here?
  eval $ "case-split-inline 'xs"
  eval $ "any-call (unfold 'sum)"
  eval $ "any-call (unfold 'length)"
  apply simplify
  eval "case-alt 1"
  apply $ alphaAlt ["y", "ys"]
  eval "alt-rhs"

  eval "{"
  setPath $ arg 3
  setPath $ arg 3
  apply $ letIntro "l"
  eval "}"

  eval "{"
  setPath $ arg 2
  setPath $ arg 3
  apply $ letIntro "s"
  eval "}"

  apply $ innermost letFloat
  apply . try $ reorderLets ["s", "l"]
  apply $ letTuple "sl"

  eval "{"
  eval "case-expr"
  apply $ foldRemembered "sumlen"
  eval "}"

  eval "}"

