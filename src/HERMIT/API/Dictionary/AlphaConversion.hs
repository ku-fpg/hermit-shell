{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.AlphaConversion where

import Data.Aeson

import HERMIT.API.Types



-- | Renames the bound variables at the current node.
alpha :: Rewrite LCore
alpha = Transform $ method "alpha" []

-- | Renames the bound variable in a Lambda expression.  Optionally accepts a new name to use.
alphaLam :: Maybe String -> Rewrite LCore
alphaLam mstr = Transform $ method "alphaLam" [toJSON mstr]

-- | Renames the binder in a Case expression to the given name.  Optionally accepts a new name to use.
alphaCaseBinder :: Maybe String -> Rewrite LCore
alphaCaseBinder mstr = Transform $ method "alphaCaseBinder" [toJSON mstr]

-- | Renames all binders in a Case alternative.
alphaAlt :: Rewrite LCore
alphaAlt = Transform $ method "alphaAlt" []

-- | Renames all binders in a Case alternative.  Accepts a list of new names to use.
alphaAltWith :: [String] -> Rewrite LCore
alphaAltWith strs = Transform $ method "alphaAltWith" [toJSON strs]

-- | Renames all binders in a Case alternative.
alphaCase :: Rewrite LCore
alphaCase = Transform $ method "alphaCase" []

-- | Renames the bound variables in a Let expression.
alphaLet :: Rewrite LCore
alphaLet = Transform $ method "alphaLet" []

-- | Renames the bound variables in a Let expression.  Accepts a list of new names to use.
alphaLetWith :: [String] -> Rewrite LCore
alphaLetWith strs = Transform $ method "alphaLetWith" [toJSON strs]

-- | Renames the bound variables in a top-level binding group at the head of the program.
alphaTop :: Rewrite LCore
alphaTop = Transform $ method "alphaTop" []

-- | Renames the bound variables in a top-level binding group at the head of the program.  Accepts a list of new names to use.
alphaTopWith :: [String] -> Rewrite LCore
alphaTopWith strs = Transform $ method "alphaTopWith" [toJSON strs]

-- | Rename all top-level identifiers in the program.
alphaProg :: Rewrite LCore
alphaProg = Transform $ method "alphaProg" []

-- | Rename local variables with manifestly unique names (x, x0, x1, ...).
unshadow :: Rewrite LCore
unshadow = Transform $ method "unshadow" []
