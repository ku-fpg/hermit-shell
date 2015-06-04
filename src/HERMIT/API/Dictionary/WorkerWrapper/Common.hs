{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.WorkerWrapper.Common where

import HERMIT.API.Types
import Data.Aeson

-- | Introduce a lemma for worker/wrapper assumption A
--   using given abs and rep functions.
introWWAssumptionA :: LemmaName -> String -> String -> Transform LCore ()
introWWAssumptionA nm absC repC
  = Transform $ method "introWWAssumptionA"
                       [ toJSON nm
                       , toJSON absC
                       , toJSON repC
                       ]

-- | Introduce a lemma for worker/wrapper assumption B
--   using given abs, rep, and body functions.
introWWAssumptionB :: LemmaName -> String -> String -> String -> Transform LCore ()
introWWAssumptionB nm absC repC bodyC
  = Transform $ method "introWWAssumptionB"
                       [ toJSON nm
                       , toJSON absC
                       , toJSON repC
                       , toJSON bodyC
                       ]

-- | Introduce a lemma for worker/wrapper assumption C
--   using given abs, rep, and body functions.
introWWAssumptionC :: LemmaName -> String -> String -> String -> Transform LCore ()
introWWAssumptionC nm absC repC bodyC
  = Transform $ method "introWWAssumptionC"
                       [ toJSON nm
                       , toJSON absC
                       , toJSON repC
                       , toJSON bodyC
                       ]

-- | split1Beta <name> <abs expression> <rep expression>
--   Perform worker/wrapper split with condition 1-beta.
--   Given lemma name argument is used as prefix to two introduced lemmas.
--     <name>-assumption: unproven lemma for w/w assumption C.
--     <name>-fusion: assumed lemma for w/w fusion.
split1Beta :: LemmaName -> String -> String -> Rewrite LCore
split1Beta nm absC rep
  = Transform $ method "split1Beta"
                       [ toJSON nm
                       , toJSON absC
                       , toJSON rep
                       ]

-- | split2Beta <name> <abs expression> <rep expression>
--   Perform worker/wrapper split with condition 2-beta.
--   Given lemma name argument is used as prefix to two introduced lemmas.
--     <name>-assumption: unproven lemma for w/w assumption C.
--     <name>-fusion: assumed lemma for w/w fusion.
split2Beta :: LemmaName -> String -> String -> Rewrite LCore
split2Beta nm absC rep
  = Transform $ method "split2Beta"
                       [ toJSON nm
                       , toJSON absC
                       , toJSON rep
                       ]

