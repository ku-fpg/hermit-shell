module WWAssBScript where

import HERMIT.API
import HERMIT.API.Types

wwb :: Rewrite LCore
wwb =
  etaExpand "xs"
  >>> rewriteCrumb lamBody
  >>> unfoldWith "wrap"
  >>> rewriteCrumb (caseAlt 1) >>> rewriteCrumb altRhs
    >>> unfoldWith "unwrap"
    >>> unfoldWith "f" >>> bash
  >>> fold "f"
  >>> etaReduce

