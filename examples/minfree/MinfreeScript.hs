module MinfreeScript where

import Prelude hiding (repeat)
import Control.Monad (replicateM_)

import HERMIT.API
import HERMIT.API.Types

ruleToLemma :: String -> Shell ()
ruleToLemma s = eval $ "rule-to-lemma " ++ show s

assumeLemma :: String -> Shell ()
assumeLemma s = do
  shellEffect $ proveLemma (LemmaName s)
  proofCmd assume

diffPartition :: Shell ()
diffPartition = do
  mapM_ (query . modifyLemma "diff-partition")
        [ instDictionaries
        ]

  assumeLemma "diff-partition"

script :: Shell ()
script = do
  mapM_ (\s -> ruleToLemma s >> assumeLemma s)
        [ "head-++"
        , "const-elim"
        ]

  -- ruleToLemma "minfrees-are-eqv"
  ruleToLemma "minfree-const-eqv"

  ruleToLemma "diff-partition"
  diffPartition

  apply flattenModule

  proof "minfree-const-eqv" $
    apply $ repeat (anyTD (unfoldAny ["minfree", "minfree'", "const"]))

  query $ instLemma "minfree-const-eqv" "b" "chooseB xs"

  setPath $ rhsOf "main"
  apply . anyBU $ lemmaBackward "minfree-const-eqv"

  apply . anyBU $ unfoldWith "minfree'"
  replicateM_ 2 . apply
                $ serialise [anyTD betaReduce, anyTD letSubst]


  -- proof "minfrees-are-eqv" $ do
  --   pathS [forallBody] $ do
  --     pathS [eqRhs] $ do
  --       apply $ unfoldWith "minfree"
  --       pathS [appArg] $ do return ()
  --   stopScript
          -- apply $ lemmaForward "diff-partition"

