{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Inline (
      inline
    , inlineWith
    , InlineArgs
    , inlineCaseScrutinee
    , inlineCaseAlternative
    ) where

import Data.Aeson

import HERMIT.API.Types

-- | (Var v) ==> <defn of v>
inline :: Rewrite LCore
inline = Transform $ method "inline" []

-- |
-- inlineWith :: Name -> Rewrite LCore
--   Given a specific v, (Var v) ==> <defn of v>
-- inlineWith :: [String] -> Rewrite LCore
--   If the current variable matches any of the given names, then inline it.
inlineWith :: InlineArgs a => a -> Rewrite LCore
inlineWith = Transform . inlineMethod

-- | Class of types that can be used as an argument to 'inlineWith'.
class ToJSON a => InlineArgs a where
    inlineMethod :: a -> Value

instance InlineArgs Name where
    inlineMethod nm = method "inlineWith" [toJSON nm]

instance InlineArgs [String] where
    inlineMethod nms = method "inlineAny" [toJSON nms]

-- | if v is a case binder, replace (Var v) with the bound case scrutinee.
inlineCaseScrutinee :: Rewrite LCore
inlineCaseScrutinee = Transform $ method "inlineCaseScrutinee" []

-- | if v is a case binder, replace (Var v) with the bound case-alternative pattern.
inlineCaseAlternative :: Rewrite LCore
inlineCaseAlternative = Transform $ method "inlineCaseAlternative" []
