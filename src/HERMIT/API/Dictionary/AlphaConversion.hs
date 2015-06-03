{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.AlphaConversion where

-- import Data.Aeson
import HERMIT.API.Types

-- | Renames the bound variables at the current node.
alpha :: Rewrite LCore
alpha = Transform $ method "alpha" []

-- -- | Renames the bound variable in a Lambda expression to the given name.
-- alphaLam :: String -> Rewrite LCore
-- 
-- -- | Renames the bound variable in a Lambda expression.
-- alphaLam :: Rewrite LCore
-- 
-- -- | Renames the binder in a Case expression to the given name.
-- alphaCaseBinder :: String -> Rewrite LCore
-- 
-- -- | Renames the binder in a Case expression.
-- alphaCaseBinder :: Rewrite LCore

-- -- | Renames all binders in a Case alternative.
-- alphaAlt :: Rewrite LCore

-- -- | Renames all binders in a Case alternative using the user-provided list of new names.
-- alphaAlt :: [String] -> Rewrite LCore

-- | Renames all binders in a Case alternative.
alphaCase :: Rewrite LCore
alphaCase = Transform $ method "alphaCase" []

-- -- | Renames the bound variables in a Let expression using a list of suggested names.
-- alphaLet :: [String] -> Rewrite LCore

-- -- | Renames the bound variables in a Let expression.
-- alphaLet :: Rewrite LCore

-- -- | Renames the bound identifiers in the top-level binding group at the head of the program using a list of suggested names.
-- alphaTop :: [String] -> Rewrite LCore

-- -- | Renames the bound identifiers in the top-level binding at the head of the program.
-- alphaTop :: Rewrite LCore

-- | Rename all top-level identifiers in the program.
alphaProg :: Rewrite LCore
alphaProg = Transform $ method "alphaProg" []

-- | Rename local variables with manifestly unique names (x, x0, x1, ...).
unshadow :: Rewrite LCore
unshadow = Transform $ method "unshadow" []