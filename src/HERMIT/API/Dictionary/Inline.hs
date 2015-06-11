{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Inline where

import Data.Aeson

import HERMIT.API.Types

-- | (Var v) ==> <defn of v>
inline :: Rewrite LCore
inline = Transform $ method "inline" []

-- | Given a specific v, (Var v) ==> <defn of v>
inlineWith :: String -> Rewrite LCore
inlineWith nm = Transform $ method "inlineWith" [toJSON nm] 

-- | If the current variable matches any of the given names, then inline it.
inlineAny :: [String] -> Rewrite LCore
inlineAny nms = Transform $ method "inlineAny" [toJSON nms]

-- | if v is a case binder, replace (Var v) with the bound case scrutinee.
inlineCaseScrutinee :: Rewrite LCore
inlineCaseScrutinee = Transform $ method "inlineCaseScrutinee" []

-- | if v is a case binder, replace (Var v) with the bound case-alternative pattern.
inlineCaseAlternative :: Rewrite LCore
inlineCaseAlternative = Transform $ method "inlineCaseAlternative" []
