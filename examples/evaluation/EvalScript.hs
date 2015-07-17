{-# LANGUAGE NoImplicitPrelude #-}
module EvalScript where

import HERMIT.API.Prelude

import WWAssAScript

script :: Shell ()
script = do
  apply flattenModule
  setPath $ bindingOf "eval"
  apply $ wwResultSplit "abs" "rep" (wwResultAssAToAssC wwa)

  scope $ do
    sendCrumb defRhs
    apply letSubst

    scope $ do
      sendCrumb letBody
      apply $ alphaLamWith "e"
      sendCrumb lamBody
      apply $ unfoldWith "abs"

    scope $ do
      setPath $ rhsOf "work"
      apply $ alphaLamWith "e" ; sendCrumb lamBody
      apply $ unfoldWith "rep"
      apply $ bash
      sendCrumb lamBody; sendCrumb lamBody -- XXX: Is this right?
      scope $ do
        setPath (consider CaseOf) ; sendCrumb (caseAlt 1) ; sendCrumb altRhs ; apply $ abstract "m"
        setPath (consider CaseOf) ; sendCrumb (caseAlt 1) ; sendCrumb altRhs ; apply $ abstract "n"
      apply $ anyBU (fold "rep")
      apply $ anyTD (forward wwResultFusion)

