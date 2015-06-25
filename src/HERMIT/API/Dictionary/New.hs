{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.New where

import Data.Aeson
import HERMIT.API.Types

{-| 
  var '<v> returns successfully for variable v, and fails otherwise.

  Useful in combination with \"when\", as in: when (var v) r
-}
var :: String -> Transform LCore ()
var v = Transform $ method "var" [toJSON v]

{-| 
  Introduce a new non-recursive binding.  
  Only works at Expression or Program nodes.

  nonrec-into 'v [| e |]
  body ==> let v = e in body
-}
nonrecIntro :: String -> String -> Rewrite LCore
nonrecIntro str cStr = Transform $ method "nonrecIntro" 
    [toJSON str, toJSON cStr]
