{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Induction where

import Data.Aeson
import HERMIT.API.Types

-- | Induct on specified value quantifier.
induction :: Name -> Rewrite LCore
induction nm = Transform $ method "induction" [toJSON True, toJSON nm]

-- | Case split on specified value quantifier.
proveByCases :: Name -> Rewrite LCore
proveByCases nm = Transform $ method "induction" [toJSON False, toJSON nm]
