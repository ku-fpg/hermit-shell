{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Reasoning (
      retraction
    , retractionUnsafe
    , unshadowQuantified
    , mergeQuantifiers
    , floatLeft
    , floatRight
    , conjunct
    , disjunct
    , imply
    , lemmaBirewrite
    , lemmaForward
    , lemmaBackward
    , lemmaConsequent
    , lemmaConsequentBirewrite
    , lemmaLhsIntro
    , lemmaRhsIntro
    , instLemma
    , instDictionaries
    , abstractForall
    , AbstractForallArgs
    , copyLemma
    , modifyLemma
    , queryLemma
    , extensionalityWith
    , extensionality
    , lhs
    , LHSArgs
    , rhs
    , RHSArgs
    , both
    , BothArgs
    , reflexivity
    , simplifyLemma
    , splitAntecedent
    , lemma
    ) where

import Data.Aeson

import HERMIT.API.Types

-- | Given f :: X -> Y and g :: Y -> X, and a proof that f (g y) ==> y, then
--   f (g y) <==> y.
retraction :: String -> String -> Rewrite LCore -> BiRewrite LCore
retraction f g r
  = BiTransform $ method "retraction"
                         [ toJSON f
                         , toJSON g
                         , toJSON r
                         ]

-- | Given f :: X -> Y and g :: Y -> X, then
--   f (g y) <==> y.
--   Note that the precondition (f (g y) == y) is expected to hold.
-- retractionUnsafe :: String -> String -> BiRewrite LCore
retractionUnsafe :: String -> String -> BiRewrite LCore
retractionUnsafe f g
  = BiTransform $ method "retractionUnsafe"
                         [ toJSON f
                         , toJSON g
                         ]

-- | Unshadow a quantified clause.
unshadowQuantified :: Rewrite LCore
unshadowQuantified = Transform $ method "unshadowQuantified" []

-- | Merge quantifiers from two clauses if they have the same type.
--   Example:
--   (forall (x::Int). foo x = x) ^ (forall (y::Int). bar y y = 5)
--   merge-quantifiers 'x 'y
--   forall (x::Int). (foo x = x) ^ (bar x x = 5)
--   Note: if only one quantifier matches, it will be floated if possible.
mergeQuantifiers :: HermitName -> HermitName -> Rewrite LCore
mergeQuantifiers n1 n2
  = Transform $ method "mergeQuantifiers"
                       [ toJSON n1
                       , toJSON n2
                       ]

-- | Float quantifier out of left-hand side.
floatLeft :: HermitName -> Rewrite LCore
floatLeft n = Transform $ method "floatLeft" [toJSON n]

-- | Float quantifier out of right-hand side.
floatRight :: HermitName -> Rewrite LCore
floatRight n = Transform $ method "floatRight" [toJSON n]

-- | conjunct new-name lhs-name rhs-name
conjunct :: LemmaName -> LemmaName -> LemmaName -> Transform LCore ()
conjunct n1 n2 n3
  = Transform $ method "conjunct"
                       [ toJSON n1
                       , toJSON n2
                       , toJSON n3
                       ]

-- | disjunct new-name lhs-name rhs-name
disjunct :: LemmaName -> LemmaName -> LemmaName -> Transform LCore ()
disjunct n1 n2 n3
  = Transform $ method "disjunct"
                       [ toJSON n1
                       , toJSON n2
                       , toJSON n3
                       ]

-- | imply new-name antecedent-name consequent-name
imply :: LemmaName -> LemmaName -> LemmaName -> Transform LCore ()
imply n1 n2 n3
  = Transform $ method "imply"
                       [ toJSON n1
                       , toJSON n2
                       , toJSON n3
                       ]
-- | Generate a bi-directional rewrite from a lemma.
lemmaBirewrite :: LemmaName -> BiRewrite LCore
lemmaBirewrite n = BiTransform $ method "lemmaBirewrite" [toJSON n]

-- | Generate a rewrite from a lemma, left-to-right.
lemmaForward :: LemmaName -> Rewrite LCore
lemmaForward n = Transform $ method "lemmaForward" [toJSON n]

-- | Generate a rewrite from a lemma, right-to-left.
lemmaBackward :: LemmaName -> Rewrite LCore
lemmaBackward n = Transform $ method "lemmaBackward" [toJSON n]

-- | Match the current lemma with the consequent of an implication lemma.
--   Upon success, replaces with antecedent of the implication, properly instantiated.
lemmaConsequent :: LemmaName -> Rewrite LCore
lemmaConsequent n = Transform $ method "lemmaConsequent" [toJSON n]

-- | Generate a bi-directional rewrite from the consequent of an implication lemma.
--   The antecedent is instantiated and introduced as an unproven obligation.
lemmaConsequentBirewrite :: LemmaName -> BiRewrite LCore
lemmaConsequentBirewrite n
  = BiTransform $ method "lemmaConsequentBiRewrite" [toJSON n]

-- | Introduce the LHS of a lemma as a non-recursive binding, in either an expression or a program.
--   body ==> let v = lhs in body
lemmaLhsIntro :: LemmaName -> Rewrite LCore
lemmaLhsIntro n = Transform $ method "lemmaLhsIntro" [toJSON n]

-- | Introduce the RHS of a lemma as a non-recursive binding, in either an expression or a program.
--   body ==> let v = rhs in body
lemmaRhsIntro :: LemmaName -> Rewrite LCore
lemmaRhsIntro n = Transform $ method "lemmaRhsIntro" [toJSON n]

-- | Instantiate one of the universally quantified variables of the given lemma,
--   with the given Core expression, creating a new lemma. Instantiating an
--   already proven lemma will result in the new lemma being considered proven.
instLemma :: LemmaName -> HermitName -> String -> Transform LCore ()
instLemma nm v cs
  = Transform $ method "instLemma"
                       [ toJSON nm
                       , toJSON v
                       , toJSON cs
                       ]

-- | Instantiate all of the universally quantified dictionaries of the given lemma.
instDictionaries :: Rewrite LCore
instDictionaries = Transform $ method "instDictionaries" []

-- |
-- abstractForall :: String -> CoreString -> Rewrite LCore
--   Weaken a lemma by abstracting an expression to a new quantifier.
-- abstractForall :: String -> Rewrite LCore -> Rewrite LCore
--   Weaken a lemma by abstracting an expression to a new quantifier.
abstractForall :: AbstractForallArgs a => String -> a -> Rewrite LCore
abstractForall str = Transform . abstractForallMethod str

class AbstractForallArgs a where
    abstractForallMethod :: String -> a -> Value

instance AbstractForallArgs CoreString where
    abstractForallMethod str cstr = method "abstractForall"
        [toJSON str, toJSON cstr]

instance AbstractForallArgs (Rewrite LCore) where
    abstractForallMethod str cstr = method "abstractForallExtract"
        [toJSON str, toJSON cstr]

-- | Copy a given lemma, with a new name.
copyLemma :: LemmaName -> LemmaName -> Transform LCore ()
copyLemma nm newName
  = Transform $ method "copyLemma"
                       [ toJSON nm
                       , toJSON newName
                       ]

-- | Modify a given lemma. Resets proven status to Not Proven and used status to Not Used.
modifyLemma :: LemmaName -> Rewrite LCore -> Transform LCore ()
modifyLemma nm rr
  = Transform $ method "modifyLemma"
                       [ toJSON nm
                       , toJSON rr
                       ]

-- | Apply a transformation to a lemma, returning the result.
queryLemma :: LemmaName -> Transform LCore String -> Transform LCore String
queryLemma nm t
  = Transform $ method "queryLemma"
                       [ toJSON nm
                       , toJSON t
                       ]

--showLemma :: PrettyPrinter -> LemmaName -> Pretty LCore

--showLemmas :: PrettyPrinter -> LemmaName -> Pretty LCore
--showLemmas :: PrettyPrinter -> Pretty LCore

-- | Given a name 'x, then
--   f == g  ==>  forall x.  f x == g x
extensionalityWith :: String -> Rewrite LCore
extensionalityWith nm = Transform $ method "extensionalityWith" [toJSON nm]

-- | f == g  ==>  forall x.  f x == g x
extensionality :: Rewrite LCore
extensionality = Transform $ method "extensionality" []

-- |
-- lhs :: Transform LCore String -> Transform LCore String
--   Apply a transformation to the LHS of a quantified clause.
-- lhs :: Rewrite LCore -> Rewrite LCore
--   Apply a rewrite to the LHS of a quantified clause.
lhs :: LHSArgs a b => Transform a b -> Transform a b
lhs = lhsTransform

class LHSArgs a b where
    lhsTransform :: Transform a b -> Transform a b
    lhsTransform t = Transform $ method "lhs" [toJSON t]

instance LHSArgs LCore String
instance LHSArgs LCore LCore

-- |
-- rhs :: Transform LCore String -> Transform LCore String
--   Apply a transformation to the RHS of a quantified clause.
-- rhs :: Rewrite LCore -> Rewrite LCore
--   Apply a rewrite to the RHS of a quantified clause.
rhs :: RHSArgs a b => Transform a b -> Transform a b
rhs = rhsTransform

class RHSArgs a b where
    rhsTransform :: Transform a b -> Transform a b
    rhsTransform t = Transform $ method "rhs" [toJSON t]

instance RHSArgs LCore String
instance RHSArgs LCore LCore

-- |
-- both :: Transform LCore String -> Transform LCore String
--   Apply a transformation to both sides of a quantified clause.
-- both :: Rewrite LCore -> Rewrite LCore
--   Apply a rewrite to both sides of an equality, succeeding if either succeed.
both :: BothArgs a b => Transform a b -> Transform a b
both = bothTransform

class BothArgs a b where
    bothTransform :: Transform a b -> Transform a b
    bothTransform t = Transform $ method "both" [toJSON t]

instance BothArgs LCore String
instance BothArgs LCore LCore

-- | Rewrite alpha-equivalence to true.
reflexivity :: Rewrite LCore
reflexivity = Transform $ method "reflexivity" []

-- | Reduce a proof by applying reflexivity and logical operator identities.
simplifyLemma :: Rewrite LCore
simplifyLemma = Transform $ method "simplifyLemma" []

-- | Split an implication of the form (q1 ^ q2) => q3 into q1 => (q2 => q3)
splitAntecedent :: Rewrite LCore
splitAntecedent = Transform $ method "splitAntecedent" []

-- | Rewrite clause to true using given lemma.
lemma :: LemmaName -> Rewrite LCore
lemma nm = Transform $ method "lemma" [toJSON nm]

-- XXX: Should this be exposed?
--lemmaUnsafe :: LemmaName -> Rewrite LCore
--lemmaUnsafe nm = Transform $ method "lemmaUnsafe" [toJSON nm]

