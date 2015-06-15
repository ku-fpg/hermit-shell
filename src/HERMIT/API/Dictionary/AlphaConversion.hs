{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.AlphaConversion where

import Data.Aeson

import HERMIT.API.Types


-- | Renames the bound variables at the current node.
alpha :: Rewrite LCore
alpha = Transform $ method "alpha" []

-- | Renames the bound variable in a Lambda expression.
alphaLam :: Rewrite LCore
alphaLam = Transform $ method "alphaLam" [toJSON (Nothing :: Maybe String)]

{-| 
  Renames the bound variable in a Lambda expression, specifying a new name to 
  use.
-}
alphaLamWith :: String -> Rewrite LCore
alphaLamWith mstr = Transform $ method "alphaLam" [toJSON (Just mstr)]

-- | Renames the binder in a Case expression.
alphaCaseBinder :: Rewrite LCore
alphaCaseBinder =  
    Transform $ method "alphaCaseBinder" [toJSON (Nothing :: Maybe String)]

-- | Renames the binder in a Case expression, specifying a new name to use.
alphaCaseBinderWith :: String -> Rewrite LCore
alphaCaseBinderWith mstr = 
    Transform $ method "alphaCaseBinder" [toJSON (Just mstr)]

-- | Renames all binders in a Case alternative.
alphaAlt :: Rewrite LCore
alphaAlt = Transform $ method "alphaAlt" []

{-| 
  Renames all binders in a Case alternative, specifying a list of new names to 
  use.
-}
alphaAltWith :: [String] -> Rewrite LCore
alphaAltWith strs = Transform $ method "alphaAltWith" [toJSON strs]

-- | Renames all binders in a Case alternative.
alphaCase :: Rewrite LCore
alphaCase = Transform $ method "alphaCase" []

-- | Renames the bound variables in a Let expression.
alphaLet :: Rewrite LCore
alphaLet = Transform $ method "alphaLet" []

{-| 
  Renames the bound variables in a Let expression.  
  Accepts a list of new names to use.
-}
alphaLetWith :: [String] -> Rewrite LCore
alphaLetWith strs = Transform $ method "alphaLetWith" [toJSON strs]

{-| 
  Renames the bound variables in a top-level binding group at the head of the 
  program.
-}
alphaTop :: Rewrite LCore
alphaTop = Transform $ method "alphaTop" []

{-| 
  Renames the bound variables in a top-level binding group at the head of the 
  program.  Accepts a list of new names to use.
-}
alphaTopWith :: [String] -> Rewrite LCore
alphaTopWith strs = Transform $ method "alphaTopWith" [toJSON strs]

-- | Rename all top-level identifiers in the program.
alphaProg :: Rewrite LCore
alphaProg = Transform $ method "alphaProg" []

-- | Rename local variables with manifestly unique names (x, x0, x1, ...).
unshadow :: Rewrite LCore
unshadow = Transform $ method "unshadow" []
