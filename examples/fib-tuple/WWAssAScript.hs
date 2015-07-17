{-# LANGUAGE NoImplicitPrelude #-}
module WWAssAScript where

import HERMIT.API.Prelude

wwa :: Rewrite LCore
wwa =
  serialise
  [
    -- wrap (unwrap h)
    etaExpand "n"
    -- \ n -> wrap (unwrap h) n
    , anyCall (unfoldWith "wrap")
    -- \ n -> fst (unwrap h n)
    , anyCall (unfoldWith "unwrap")
    -- \ n -> fst (h n, h (S n))
    , bash
    -- h
  ]

