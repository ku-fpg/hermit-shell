{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HERMIT.API.Types where
        
import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text
import Data.String
import Data.Typeable

import HERMIT.GHCI.JSON

import Data.Coerce

------------------------------------------------------------------------

data Shell :: * -> * where
  Shell :: FromJSON a => Value -> Shell a
  Bind   :: Shell a -> (a -> Shell b) -> Shell b
  Return :: a -> Shell a
  Fail   :: String -> Shell a
  
instance Functor Shell where
  fmap f s = pure f <*> s
  
instance Applicative Shell where
  pure a = Return a
  (<*>) = liftM2 ($)

instance Monad Shell where
  return a = Return a
  (>>=) = Bind

toShell   :: Shell a -> Value
toShell (Shell v) = v

fromShell :: Shell a -> Value -> ShellResult a
fromShell (Shell {}) = fromJust . parseMaybe parseJSON

data ShellResult a
  = ShellResult [[Glyph]] a -- When was said, what was returned
  | ShellFailure String     -- something went wrong
  | ShellAbort              -- HERMIT has returned control to GHCI;
                            -- please stop sending messages.
  | ShellResume             -- Resume compilation
    deriving Show

instance FromJSON a => FromJSON (ShellResult a) where
  parseJSON (Object o) = ShellResult <$> o .: "output"
                                     <*> o .: "result"
                      <|> parseResume
                      <|> return (ShellFailure "malformed Object returned from Server")
    where
      parseResume = do
        "resume" :: String <- o .: "shutdown"
        return ShellResume
  parseJSON _ = return (ShellFailure "Object not returned from Server")


------------------------------------------------------------------------

-- | The 'Guts' of GHC, is anything we can rewrite and transform.
class Typeable a => Guts a

proxyToJSON :: forall (a :: *) . Typeable a => Proxy a -> Value
proxyToJSON Proxy = String $ pack $ show $ typeOf (undefined :: a)

------------------------------------------------------------------------
-- | The 'Response' of doing a 'Shell' effect.

class FromJSON a => Response a where
  showResponse :: a -> String 

instance Response () where
  showResponse () = ""

------------------------------------------------------------------------

type Rewrite a = Transform a a

newtype Transform a b = Transform Value
  deriving ToJSON

------------------------------------------------------------------------

type BiRewrite a = BiTransform a a

newtype BiTransform a b = BiTransform Value
  deriving ToJSON

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

instance Response LocalPath where
  showResponse (LocalPath txt) = show txt

------------------------------------------------------------------------

data LCoreTC = LCoreTC
  deriving Typeable
instance Guts LCoreTC

data LCore = LCore
  deriving Typeable
  
instance Guts LCore 

------------------------------------------------------------------------

newtype KernelEffect = KernelEffect Value

------------------------------------------------------------------------

newtype ShellEffect = ShellEffect Value

------------------------------------------------------------------------

newtype QueryFun = QueryFun Value

------------------------------------------------------------------------

newtype AST = AST Int
    deriving (Num, Enum, Ord, Eq, Integral, Real)

instance Show AST where
    show (AST n) = show n

instance Read AST where
    readsPrec n = fmap (first (coerce :: Int -> AST)) . readsPrec n
      where
        first f (x, y) = (f x, y)

------------------------------------------------------------------------

data PpType = Show | Abstract | Omit
    deriving (Show, Read)

------------------------------------------------------------------------

newtype ScriptEffect = ScriptEffect Value

------------------------------------------------------------------------

type RewriteH   a = Rewrite a
type BiRewriteH a = BiRewrite a

------------------------------------------------------------------------

newtype CommandLineState = CommandLineState Value
  deriving ToJSON

------------------------------------------------------------------------

method :: Text -> [Value] -> Value
method nm params = object ["method" .= nm, "params" .= params]



