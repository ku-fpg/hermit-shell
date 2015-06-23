module WWAssBScript where

import HERMIT.API
import HERMIT.API.Types

wwb :: Rewrite LCore
wwb =
  etaExpand "xs"
  >>> pathS [lamBody]
    ( unfoldWith "wrap"
      >>> pathS [caseAlt 1, altRhs]
                ( unfoldWith "unwrap"
                  >>> unfoldWith "f" >>> bash
                )
      >>> fold "f"
    )
  >>> etaReduce

