{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module HERMIT.API.Dictionary.AlphaConversion where

import HERMIT.API.Types


-- | Renames the bound variables at the current node.
alpha :: Rewrite LCore
alpha = Transform $ method "alpha" []

-- | Renames the bound variable in a Lambda expression.  Optionally accepts a new name to use.
alphaLam :: (ReturnType a ~ Rewrite LCore, RewriteWithString a) => a
alphaLam = rewriteWithString "alphaLam"

-- | Renames the binder in a Case expression to the given name.  Optionally accepts a new name to use.
alphaCaseBinder :: (ReturnType a ~ Rewrite LCore, RewriteWithString a) => a
alphaCaseBinder = rewriteWithString "alphaCaseBinder"

-- | Renames all binders in a Case alternative.  Optionally accepts a list of new names to use.
alphaAlt :: (ReturnType a ~ Rewrite LCore, RewriteWithStrings a) => a
alphaAlt = rewriteWithStrings "alphaAlt"

-- | Renames all binders in a Case alternative.
alphaCase :: Rewrite LCore
alphaCase = Transform $ method "alphaCase" []

-- | Renames the bound variables in a Let expression.  Optionally accepts a list of new names to use.
alphaLet :: (ReturnType a ~ Rewrite LCore, RewriteWithStrings a) => a
alphaLet = rewriteWithStrings "alphaLet"

-- | Renames the bound identifiers in the top-level binding group at the head of the program using a list of suggested names.
alphaTop :: (ReturnType a ~ Rewrite LCore, RewriteWithStrings a) => a
alphaTop = rewriteWithStrings "alphaTop"

-- | Rename all top-level identifiers in the program.
alphaProg :: Rewrite LCore
alphaProg = Transform $ method "alphaProg" []

-- | Rename local variables with manifestly unique names (x, x0, x1, ...).
unshadow :: Rewrite LCore
unshadow = Transform $ method "unshadow" []
