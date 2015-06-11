{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Debug where

import Data.Aeson
import HERMIT.API.Types

-- | give a side-effect message as output when processing this command
trace :: String -> Rewrite LCoreTC
trace str = Transform $ method "trace" [toJSON str]

-- | give a side-effect message as output, and observe the value being processed
observe :: String -> Rewrite LCoreTC
observe str = Transform $ method "observe" [toJSON str]

-- | give a side-effect message if the rewrite fails, including the failing input
observeFailure :: String -> Rewrite LCoreTC -> Rewrite LCoreTC
observeFailure str r = Transform $ method "observeFailure" 
    [toJSON str, toJSON r]

-- | if given rewrite succeeds, see its input and output
bracket :: String -> Rewrite LCoreTC -> Rewrite LCoreTC
bracket str r = Transform $ method "bracket" [toJSON str, toJSON r]
