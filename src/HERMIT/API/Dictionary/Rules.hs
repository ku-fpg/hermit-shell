{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Rules where

import HERMIT.API.Types
import Data.Aeson

-- | List all the rules in scope.
showRules :: Transform LCore String
showRules = Transform $ method "showRules" []

{-
-- | Display details on the named rule.
showRule :: PrettyPrinter -> RuleName -> Transform LCoreTC Doc
showRule pp rule
  = Transform $ method "showRule"
                       [ toJSON pp
                       , toJSON rule
                       ]
-}

-- | Apply a named GHC rule right-to-left.
foldRule :: RuleName -> Rewrite LCore
foldRule name = Transform $ method "foldRule" [ toJSON name ]

-- | Apply named GHC rules right-to-left, succeed if any of the rules succeed.
foldRules :: [RuleName] -> Rewrite LCore
foldRules names = Transform $ method "foldRules" [ toJSON names ]

-- | Apply a named GHC rule left-to-right.
unfoldRule :: RuleName -> Rewrite LCore
unfoldRule name = Transform $ method "unfoldRule" [ toJSON name ]

-- | Apply a named GHC rule left-to-right.
unfoldRuleUnsafe :: RuleName -> Rewrite LCore
unfoldRuleUnsafe name
  = Transform $ method "unfoldRuleUnsafe" [ toJSON name ]

-- | Apply named GHC rules left-to-right, succeed if any of the rules succeed
unfoldRules :: [RuleName] -> Rewrite LCore
unfoldRules names = Transform $ method "unfoldRules" [ toJSON names ]

-- | Apply named GHC rules left-to-right, succeed if any of the rules succeed
unfoldRulesUnsafe :: [RuleName] -> Rewrite LCore
unfoldRulesUnsafe names
  = Transform $ method "unfoldRulesUnsafe" [ toJSON names ]

{-
-- | Create a lemma from a GHC RULE.
ruleToLemma :: PrettyPrinter -> RuleName -> Transform LCore Doc
ruleToLemma pp name
  = Transform $ method "ruleToLemma"
                       [ toJSON pp
                       , toJSON name
                       ]
-}

-- | Run GHC's SpecConstr pass, which performs call pattern specialization.
specConstr :: Rewrite LCore
specConstr = Transform $ method "specConstr" []

-- | Run GHC's specialisation pass, which performs type and dictionary specialisation.
specialise :: Rewrite LCore
specialise = Transform $ method "specialise" []

