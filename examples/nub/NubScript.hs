module NubScript where
import HERMIT.API

script :: Shell ()
script = do
  shellEffect $ setPPType Show

  apply flattenModule

  setPath $ bindingOf "nub"
  apply fixIntro ; sendCrumb defRhs
  apply $ split2Beta "nub" "absN" "repN" ; proofCmd assume

  -- this bit to essentially undo the fix-intro
  scope $ do setPath (applicationOf "repN") ; sendCrumb appArg ; apply (letIntro "nub") ; apply (oneTD (unfoldWith "fix")) ; apply simplify
  apply $ innermost letFloat
  apply $ alphaLetWith ["nub'"] -- rename x to nub'

  -- back to the derivation
  setPath $ bindingOf "worker"
  apply $ oneTD (unfoldWith "repN")
  query $ remember "origworker"
  apply $ oneTD (unfoldWith "filter")
  apply $ oneTD (caseFloatArgLemma "nubStrict")

  -- prove strictness condition
  apply $ lhsR unfold ; apply smash ; proofCmd endProof

  apply $ oneTD (unfoldWith "nub'")
  apply simplify

  apply $ oneTD (caseFloatArgLemma "nubStrict")

  -- prove strictness condition
  apply $ lhsR unfold ; apply smash ; proofCmd endProof

  scope $ do setPath (consider CaseOf) ; setPath (consider CaseOf) ; sendCrumb (caseAlt 1) ; sendCrumb altRhs
             apply unfold ; apply simplify
             apply (oneTD (unfoldRule "filter-fusion")) ; proofCmd assume
             apply simplify
             apply (oneTD (unfoldRule "member-fusion")) ; proofCmd assume

  apply nonrecToRec
  apply $ anyTD (foldRemembered "origworker")

