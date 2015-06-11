{-# LANGUAGE OverloadedStrings #-}
module MeanScript where

import HERMIT.API

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

  eval "{" ; sendCrumb defRhs ; sendCrumb lamBody
  eval $ "case-split-inline 'xs"
  apply . anyCall $ unfoldWith "sum"
  apply . anyCall $ unfoldWith "length"
  apply simplify
  sendCrumb $ caseAlt 1
  apply $ alphaAltWith ["y", "ys"]
  sendCrumb $ altRhs

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
  sendCrumb caseExpr
  apply $ foldRemembered "sumlen"
  eval "}"

  eval "}"

