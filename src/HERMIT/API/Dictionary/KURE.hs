{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HERMIT.API.Dictionary.KURE where

import Data.Aeson
import Data.Proxy

import HERMIT.API.Types

-- -- | Perform an identity rewrite.
-- id_ :: Rewrite LCore
--
-- -- | Perform an identity rewrite.
-- id_ :: Rewrite LCoreTC

-- | An always succeeding translation.
success :: Transform LCore ()
success = Transform $ method "success" []

-- | A failing rewrite.
fail_ :: String -> Rewrite LCore
fail_ str = Transform $ method "fail_" [toJSON str]

-- -- | Perform the first rewrite, and then, if it fails, perform the second rewrite.
-- (<+) :: Transform LCore () -> Transform LCore () -> Transform LCore ()
--
-- -- | Perform the first check, and then, if it fails, perform the second check.
-- (<+) :: Rewrite LCore -> Rewrite LCore -> Rewrite LCore
--
-- -- | Compose rewrites, requiring both to succeed.
-- (>>>) :: Rewrite LCore -> Rewrite LCore -> Rewrite LCore
--
-- -- | Compose bidirectional rewrites, requiring both to succeed.
-- (>>>) :: BiRewrite LCore -> BiRewrite LCore -> BiRewrite LCore

-- | Compose rewrites, allowing one to fail.
(>+>) :: Rewrite LCore -> Rewrite LCore -> Rewrite LCore
a >+> b = Transform . method ">+>" $ map toJSON [a, b]

-- | Try a rewrite, and perform the identity if the rewrite fails.
try :: Rewrite LCore -> Rewrite LCore
try r = Transform $ method "try" [toJSON r]

-- | Repeat a rewrite until it would fail.
repeat :: Rewrite LCore -> Rewrite LCore
repeat r = Transform $ method "repeat" [toJSON r]

-- | Repeat a rewrite n times.
replicate :: Int -> Rewrite LCore -> Rewrite LCore
replicate n r = Transform $ method "replicate" [toJSON n, toJSON r]

-- | Apply a rewrite to all children of the node, requiring success at every child.
all :: Rewrite LCore -> Rewrite LCore
all r = Transform $ method "all" [toJSON r]

-- | Apply a rewrite to all children of the node, requiring success for at least one child.
any :: Rewrite LCore -> Rewrite LCore
any r = Transform $ method "any" [toJSON r]

-- | Apply a rewrite to the first child of the node for which it can succeed.
one :: Rewrite LCore -> Rewrite LCore
one r = Transform $ method "one" [toJSON r]

-- | Promote a rewrite to operate over an entire tree in bottom-up order, requiring success at every node.
allBU :: Rewrite LCore -> Rewrite LCore
allBU r = Transform $ method "allBU" [toJSON r]

-- | Promote a rewrite to operate over an entire tree in top-down order, requiring success at every node.
allTD :: Rewrite LCore -> Rewrite LCore
allTD r = Transform $ method "allTD" [toJSON r]

-- | Apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal,
-- succeeding if they all succeed.
allDU :: Rewrite LCore -> Rewrite LCore
allDU r = Transform $ method "allDU" [toJSON r]

-- | Promote a rewrite to operate over an entire tree in bottom-up order, requiring success for at least one node.
anyBU :: Rewrite LCore -> Rewrite LCore
anyBU r = Transform $ method "anyBU" [toJSON r]

-- | Promote a rewrite to operate over an entire tree in top-down order, requiring success for at least one node.
anyTD :: Rewrite LCore -> Rewrite LCore
anyTD r = Transform $ method "anyTD" [toJSON r]

-- | Apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal,
-- succeeding if any succeed.
anyDU :: Rewrite LCore -> Rewrite LCore
anyDU r = Transform $ method "anyDU" [toJSON r]

-- | Apply a rewrite to the first node (in a top-down order) for which it can succeed.
oneTD :: Rewrite LCore -> Rewrite LCore
oneTD r = Transform $ method "oneTD" [toJSON r]

-- | Apply a rewrite to the first node (in a bottom-up order) for which it can succeed.
oneBU :: Rewrite LCore -> Rewrite LCore
oneBU r = Transform $ method "oneBU" [toJSON r]

-- | Attempt to apply a rewrite in a top-down manner, prunning at successful rewrites.
pruneTD :: Rewrite LCore -> Rewrite LCore
pruneTD r = Transform $ method "pruneTD" [toJSON r]

-- | A fixed-point traveral, starting with the innermost term.
innermost :: Rewrite LCore -> Rewrite LCore
innermost r = Transform $ method "innermost" [toJSON r]

-- -- | Apply a rewrite to a focal point.
-- focus :: Transform LCoreTC LocalPath -> Rewrite LCoreTC -> Rewrite LCoreTC)
--
-- -- | Apply a query at a focal point.
-- focus :: Transform LCoreTC LocalPath -> Transform LCoreTC String -> Transform LCoreTC String
--
-- -- | Apply a rewrite to a focal point.
-- focus :: LocalPath -> Rewrite LCoreTC -> Rewrite LCoreTC
--
-- -- | Apply a query at a focal point.
-- focus :: LocalPath -> Transform LCoreTC String -> Transform LCoreTC String
--
-- -- | Apply a rewrite to a focal point.
-- focus :: Transform LCore LocalPath -> Rewrite LCore -> Rewrite LCore
--
-- -- | Apply a query at a focal point.
-- focus :: Transform LCore LocalPath -> Transform LCore String -> Transform LCore String
--
-- -- | Apply a rewrite to a focal point.
-- focus :: LocalPath -> Rewrite LCore -> Rewrite LCore
--
-- -- | Apply a query at a focal point
-- focus :: LocalPath -> Transform LCore String -> Transform LCore String

-- | Apply a rewrite only if the check succeeds.
when :: Transform LCore () -> Rewrite LCore -> Rewrite LCore
when a b = Transform $ method "when" [toJSON a, toJSON b]

-- | Cause a failing check to succeed, a succeeding check to fail.
not_ :: Transform LCore () -> Transform LCore ()
not_ t = Transform $ method "not_" [toJSON t]

-- | Reverse a bidirectional rewrite.
invert :: BiRewrite LCore -> BiRewrite LCore
invert r = BiTransform $ method "invert" [toJSON r]

-- | Apply a bidirectional rewrite forewards.
forward :: BiRewrite LCore -> BiRewrite LCore
forward r = BiTransform $ method "forward" [toJSON r]

-- | Apply a bidirectional rewrite backwards.
backward :: BiRewrite LCore -> BiRewrite LCore
backward r = BiTransform $ method "backward" [toJSON r]

-- | Determine if a rewrite could be successfully applied.
test :: Rewrite LCore -> Rewrite LCore
test r = Transform $ method "test" [toJSON r]

-- | @any-call (.. unfold command ..)@ applies an unfold command to all applications.
--   Preference is given to applications with more arguments.
anyCall :: forall g. Guts g => Rewrite g -> Rewrite g
anyCall (Transform rr) = Transform $ method "anyCall" [proxyToJSON (Proxy :: Proxy g) , rr]

-- | Promote a RewriteCore to a RewriteCoreTC
promote :: Rewrite LCore -> Rewrite LCoreTC
promote r = Transform $ method "promote" [toJSON r]

-- -- | Extract a RewriteCore from a RewriteCoreTC"
-- extract :: Rewrite LCore -> Rewrite LCoreTC
--
-- -- | Extract a TransformLCoreString from a TransformLCoreTCString
-- extract :: TransformH LCoreTC String -> TransformH LCore String

-- | between x y rr -> perform rr at least x times and at most y times.
between :: Int -> Int -> Rewrite LCoreTC -> Rewrite LCoreTC
between x y rr = Transform $ method "between" [toJSON x, toJSON y, toJSON rr]

-- -- | return the expression found at the given path
-- atPath :: TransformH LCore LocalPathH -> TransformH LCore LCore
--
-- -- | return the expression found at the given path
-- atPath :: TransformH LCoreTC LocalPathH -> TransformH LCoreTC LCoreTC
--
-- -- | return the expression found at the given path
-- atPath :: TransformH LCoreTC LocalPathH -> TransformH LCore LCore
