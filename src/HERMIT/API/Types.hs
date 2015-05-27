{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API.Types where
        
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text

import HERMIT.GHCI.Client

data ShellEffect :: * -> * where
  ShellEffect :: Value -> ShellEffect ()

instance Shell ShellEffect where
  toShell (ShellEffect v) = v
  fromShell (ShellEffect {}) = fromJust . parseMaybe parseJSON

method :: Text -> [Value] -> Value
method nm params = object ["method" .= nm, "params" .= params]