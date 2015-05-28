{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API.Types where
        
import Control.Applicative

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text
import Data.String

import HERMIT.GHCI.JSON 

------------------------------------------------------------------------

data Shell :: * -> * where
  Shell :: FromJSON a => Value -> Shell a

toShell   :: Shell a -> Value
toShell (Shell v) = v

fromShell :: Shell a -> Value -> ShellResult a
fromShell (Shell {}) = fromJust . parseMaybe parseJSON

data ShellResult a
  = ShellResult [[Glyph]] a -- When was said, what was returned
  | ShellFailure String -- something went wrong
    deriving Show

instance FromJSON a => FromJSON (ShellResult a) where
  parseJSON (Object o) = ShellResult <$> o .: "output"
                                     <*> o .: "result"
                      <|> return (ShellFailure "malformed Object returned from Server")                                      
  parseJSON _ = return (ShellFailure "Object not returned from Server")

------------------------------------------------------------------------

newtype Name = Name String
   deriving (Eq,Ord)

instance Show Name where show (Name nm) = nm

instance IsString Name where
  fromString = Name        

instance ToJSON Name where
  toJSON (Name nm) = toJSON nm

------------------------------------------------------------------------

newtype LocalPath = LocalPath [String]

instance FromJSON LocalPath where
  parseJSON = undefined

instance ToJSON LocalPath where
  toJSON = undefined


------------------------------------------------------------------------

method :: Text -> [Value] -> Value
method nm params = object ["method" .= nm, "params" .= params]



