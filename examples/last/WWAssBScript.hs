{-# LANGUAGE NoImplicitPrelude #-}
module WWAssBScript where

import HERMIT.API.Prelude

wwb :: Rewrite LCore
wwb =
  etaExpand "xs"
  >>> pathR [lamBody]
    ( unfoldWith "wrap"
      >>> pathR [caseAlt 1, altRhs]
                ( unfoldWith "unwrap"
                  >>> unfoldWith "f" >>> bash
                )
      >>> fold "f"
    )
  >>> etaReduce

