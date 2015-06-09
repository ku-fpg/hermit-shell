{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Composite where

import Data.Aeson
import HERMIT.API.Types

-- | Unfold the current expression if it is one of the basic combinators:
-- ($), (.), id, flip, const, fst, snd, curry, and uncurry.
unfoldBasicCombinator :: Rewrite LCore
unfoldBasicCombinator = Transform $ method "unfoldBasicCombinator" []

-- | innermost (unfold-basic-combinator <+ beta-reduce-plus <+ safe-let-subst <+ case-reduce <+ let-elim)
simplify :: Rewrite LCore
simplify = Transform $ method "simplify" []

-- | See @bashHelp@ in HERMIT.
bash :: Rewrite LCore
bash = Transform $ method "bash" []

-- | See @smashHelp@ in HERMIT.
smash :: Rewrite LCore
smash = Transform $ method "smash" []

-- | Run \"bash\" extended with additional rewrites.
-- Note: be sure that the new rewrite either fails or makes progress, else this may loop.
bashExtendedWith :: [Rewrite LCore] -> Rewrite LCore
bashExtendedWith rws = Transform $ method "bashExtendedWith" [toJSON rws]

-- | Run \"smash\" extended with additional rewrites.
-- Note: be sure that the new rewrite either fails or makes progress, else this may loop.
smashExtendedWith :: [Rewrite LCore] -> Rewrite LCore
smashExtendedWith rws = Transform $ method "smashExtendedWith" [toJSON rws]

-- | verbose bash - most useful with set-auto-corelint True
bashDebug :: Rewrite LCore
bashDebug = Transform $ method "bashDebug" []
