{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Local
    ( module HERMIT.API.Dictionary.Local.Bind
    , module HERMIT.API.Dictionary.Local.Case
    , module HERMIT.API.Dictionary.Local.Cast
    , module HERMIT.API.Dictionary.Local.Let
    , betaReduce
    , betaExpand
    , etaReduce
    , etaExpand
    , flattenModule
    , flattenProgram
    , abstract
    , push
    , pushUnsafe
    ) where

import Data.Aeson

import HERMIT.API.Dictionary.Local.Bind
import HERMIT.API.Dictionary.Local.Case
import HERMIT.API.Dictionary.Local.Cast
import HERMIT.API.Dictionary.Local.Let
import HERMIT.API.Types

-- | ((\\ v -> E1) E2) ==> let v = E2 in E1
-- This form of beta-reduction is safe if E2 is an arbitrary expression
-- (won't duplicate work).
betaReduce :: Rewrite LCore
betaReduce = Transform $ method "betaReduce" []

-- | (let v = e1 in e2) ==> (\\ v -> e2) e1
betaExpand :: Rewrite LCore
betaExpand = Transform $ method "betaExpand" []

-- | (\\ v -> e1 v) ==> e1
etaReduce :: Rewrite LCore
etaReduce = Transform $ method "etaReduce" []

-- | \"eta-expand 'v\" performs e1 ==> (\\ v -> e1 v)
etaExpand :: String -> Rewrite LCore
etaExpand str = Transform $ method "etaExpand" [toJSON str]

-- | Flatten all the top-level binding groups in the module to a single recursive binding group.
-- This can be useful if you intend to appply GHC RULES.
flattenModule :: Rewrite LCore
flattenModule = Transform $ method "flattenModule" []

-- | Flatten all the top-level binding groups in a program (list of binding groups) to a single
-- recursive binding group.  This can be useful if you intend to apply GHC RULES.
flattenProgram :: Rewrite LCore
flattenProgram = Transform $ method "flattenProgram" []

-- | Abstract over a variable using a lambda.
-- e  ==>  (\\ x -> e) x
abstract :: Name -> Rewrite LCore
abstract nm = Transform $ method "abstract" [toJSON nm]

-- | Push a function 'f into a case-expression or let-expression argument,
-- given a proof that f (fully saturated with type arguments) is strict.
push :: String -> Rewrite LCore -> Rewrite LCore
push str r = Transform $ method "push" [toJSON str, toJSON r]

-- | Push a function 'f into a case-expression or let-expression argument.
-- Requires 'f to be strict.
pushUnsafe :: String -> Rewrite LCore
pushUnsafe str = Transform $ method "pushUnsafe" [toJSON str]
