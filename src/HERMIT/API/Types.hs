{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module HERMIT.API.Types where

import Control.Applicative
import Control.Monad

import Data.Coerce

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text
import Data.Typeable

import HERMIT.GHCI.Glyph

------------------------------------------------------------------------

data Shell :: * -> * where
  Shell  :: FromJSON a => Value -> Shell a
  Bind   :: Shell a -> (a -> Shell b) -> Shell b
  Return :: a -> Shell a
  Fail   :: String -> Shell a

instance Functor Shell where
  fmap f s = pure f <*> s

instance Applicative Shell where
  pure  = Return
  (<*>) = liftM2 ($)

instance Monad Shell where
  return = Return
  (>>=)  = Bind

toShell   :: Shell a -> Maybe Value
toShell (Shell v) = Just v
toShell _         = Nothing

fromShell :: Shell a -> Value -> ShellResult a
fromShell (Shell {}) v = fromJust $ parseMaybe parseJSON v
fromShell _ _          = ShellFailure "fromShell"

data ShellResult a
  = ShellResult [[Glyph]] a -- When was said, what was returned
  | ShellFailure String     -- something went wrong
  | ShellException String   -- The remote HERMIT monad failed on the server with this
    deriving Show

-- TODO: add ShellException
instance FromJSON a => FromJSON (ShellResult a) where
  parseJSON (Object o) = ShellResult <$> o .: "output"
                                     <*> o .: "result"
                      <|> return (ShellFailure "malformed Object returned from Server")
    where
  parseJSON _ = return (ShellFailure "Object not returned from Server")


------------------------------------------------------------------------

-- | The 'Guts' of GHC, is anything we can rewrite and transform.
class Typeable a => Guts a

proxyToJSON :: forall (a :: *) . Typeable a => Proxy a -> Value
proxyToJSON Proxy = String $ pack $ show $ typeOf (undefined :: a)

------------------------------------------------------------------------
-- | The 'Response' of doing a 'Shell' effect.

class FromJSON a => Response a where
  printResponse :: a -> IO ()

instance Response () where
  printResponse () = return ()

------------------------------------------------------------------------

type Rewrite a = Transform a a

newtype Transform a b = Transform Value
  deriving ToJSON

------------------------------------------------------------------------

type BiRewrite a = BiTransform a a

newtype BiTransform a b = BiTransform Value
  deriving ToJSON

------------------------------------------------------------------------

type Name = String

------------------------------------------------------------------------

newtype LocalPath = LocalPath [String]

instance FromJSON LocalPath where
  parseJSON = undefined

instance ToJSON LocalPath where
  toJSON = undefined

instance Response LocalPath where
  printResponse (LocalPath txt) = print txt

------------------------------------------------------------------------

instance Response Glyphs where
  printResponse (Glyphs gs) = do
--         putStrLn "[Start Glyphs]"
         sequence_ [ withStyle sty txt
                   | Glyph txt sty <- gs
                   ]
--         putStrLn "[End Glyphs]"

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
    deriving (Num, Enum, Ord, Eq, Integral, Real, ToJSON)

instance Show AST where
    show (AST n) = show n

instance Read AST where
    readsPrec n = fmap (first (coerce :: Int -> AST)) . readsPrec n
      where
        first f (x, y) = (f x, y)

------------------------------------------------------------------------

newtype Crumb = Crumb Value
    deriving ToJSON

------------------------------------------------------------------------

data PPType = Show | Abstract | Omit
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

type LemmaName = Name

------------------------------------------------------------------------

type HermitName = Name

------------------------------------------------------------------------

newtype Doc           = Doc Value
  deriving (ToJSON)

newtype PrettyPrinter = PrettyPrinter Value
  deriving (ToJSON)

------------------------------------------------------------------------

type RuleName = Name

------------------------------------------------------------------------

newtype ProofShellCommand = ProofShellCommand Value
  deriving (ToJSON)

------------------------------------------------------------------------

method :: Text -> [Value] -> Value
method nm params = object ["method" .= nm, "params" .= params]

------------------------------------------------------------------------

class ToJSON m => TransCat m where
    transCat :: Value -> m

instance TransCat (Transform a b) where
    transCat = Transform

instance TransCat (BiTransform a b) where
    transCat = BiTransform
