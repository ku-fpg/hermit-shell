module CaseReduceScript where

import HERMIT.API

script :: Shell ()
script = do
  scope $ do
    setPath $ rhsOf "foo"
    apply $ anyBU inline
    apply $ anyTD caseReduce

