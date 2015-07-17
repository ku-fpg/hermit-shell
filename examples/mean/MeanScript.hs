{-# LANGUAGE NoImplicitPrelude #-}
module MeanScript where

import HERMIT.API.Prelude

script :: Shell ()
script = do
  scope $ do
    setPath $ rhsOf "mean"
    sendCrumb lamBody

    scope $ do
      setPath $ arg 2
      apply $ letIntro "s"

    scope $ do
      setPath $ arg 3
      apply $ letIntro "l"

    apply $ innermost letFloat
    apply . try $ reorderLets ["s", "l"]
    apply $ letTuple "sl"

    scope $ do
      sendCrumb caseExpr
      apply $ abstract "xs"
      sendCrumb appFun
      apply $ letIntro "sumlength"


  apply $ innermost letFloat
  setPath $ bindingGroupOf "sumlength"
  apply nonrecToRec
  setPath $ bindingOf "sumlength"
  query $ remember "sumlen"

  scope $ do
    sendCrumb defRhs ; sendCrumb lamBody
    apply $ caseSplitInline "xs"
    apply . anyCall $ unfoldWith "sum"
    apply . anyCall $ unfoldWith "length"
    apply simplify
    sendCrumb $ caseAlt 1
    apply $ alphaAltWith ["y", "ys"]
    sendCrumb $ altRhs

    scope $ do
      setPath $ arg 3
      setPath $ arg 3
      apply $ letIntro "l"

    scope $ do
      setPath $ arg 2
      setPath $ arg 3
      apply $ letIntro "s"

    apply $ innermost letFloat
    apply . try $ reorderLets ["s", "l"]
    apply $ letTuple "sl"

    scope $ do
      sendCrumb caseExpr
      apply $ foldRemembered "sumlen"

