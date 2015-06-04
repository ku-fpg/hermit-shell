{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Undefined where

import HERMIT.API.Types
import Data.Aeson

-- | Set the current expression to "undefined".
replaceCurrentExprWithUndefined :: Rewrite LCore
replaceCurrentExprWithUndefined
  = Transform $ method "replaceCurrentExprWithUndefined" []


-- | Replace the specified identifier with "undefined".
replaceIdWithUndefined :: HermitName -> Rewrite LCore
replaceIdWithUndefined name
  = Transform $ method "replaceIdWithUndefined"
                       [ toJSON name ]

-- | error ty string  ==>  undefined ty
errorToUndefined :: Rewrite LCore
errorToUndefined = Transform $ method "errorToUndefined" []

-- | Succeed if the current expression is an undefined value.
isUndefinedVal :: Transform LCore ()
isUndefinedVal = Transform $ method "isUndefinedVal" []

-- | undefined-app <+ undefined-lam <+ undefined-let <+ undefined-cast <+ undefined-tick <+ undefined-case
undefinedExpr :: Rewrite LCore
undefinedExpr = Transform $ method "undefinedExpr" []

-- | (undefined ty1) e  ==>  undefined ty2
undefinedApp :: Rewrite LCore
undefinedApp = Transform $ method "undefinedApp" []

-- | (\ v -> undefined ty1)  ==>  undefined ty2   (where v is not a 'TyVar')
undefinedLam :: Rewrite LCore
undefinedLam = Transform $ method "undefinedLam" []

-- | let bds in (undefined ty)  ==>  undefined ty
undefinedLet :: Rewrite LCore
undefinedLet = Transform $ method "undefinedLet" []

-- | case (undefined ty) of alts  ==>  undefined ty
--   OR
--   case e of {pat_1 -> undefined ty ; pat_2 -> undefined ty ; ... ; pat_n -> undefined ty} ==> undefined ty
undefinedCase :: Rewrite LCore
undefinedCase = Transform $ method "undefinedCase" []

-- | Cast (undefined ty1) co  ==>  undefined ty2
undefinedCast :: Rewrite LCore
undefinedCast = Transform $ method "undefinedCast" []

-- | Tick tick (undefined ty1)  ==>  undefined ty1
undefinedTick :: Rewrite LCore
undefinedTick = Transform $ method "undefinedTick" []

