{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Debug where

import Data.Aeson
import HERMIT.API.Types

-- | give a side-effect message as output when processing this command
trace :: String -> Rewrite LCore
trace str = Transform $ method "trace" [toJSON str]

-- | give a side-effect message as output, and observe the value being processed
observe :: String -> Rewrite LCore
observe str = Transform $ method "observe" [toJSON str]

-- | give a side-effect message if the rewrite fails, including the failing input
observeFailure :: String -> Rewrite LCore -> Rewrite LCore
observeFailure str r = Transform $ method "observeFailure" [toJSON str, toJSON r]

-- | if given rewrite succeeds, see its input and output
bracket :: String -> Rewrite LCore -> Rewrite LCore
bracket str r = Transform $ method "bracket" [toJSON str, toJSON r]
