{-# LANGUAGE GADTs, KindSignatures #-}

module Transport where

import Data.Aeson
import Data.Aeson.TH
import Data.Char

import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( VarStrictType )

data Transport :: (* -> *) -> * where
  Transport :: (ToJSON (f a), ToJSON a) => f a -> Transport f

instance ToJSON (Transport f) where
  toJSON (Transport f) = toJSON f
