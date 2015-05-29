{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
module HERMIT.Server.Parser where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Applicative
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy as State

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe, Parser)
import           Data.Char (isSpace)
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text

import           HERMIT.Dictionary
import           HERMIT.External hiding (External, external)
import           HERMIT.Kernel
import           HERMIT.Kure hiding ((<$>),(<*>))

import           HERMIT.Name (mkRhsOfPred, RhsOfName(..), parseName)

import           HERMIT.Plugin
import           HERMIT.Plugin.Builder
import           HERMIT.Plugin.Types

import           HERMIT.PrettyPrinter.Common (po_width, PrettyOptions, DocH)

import           HERMIT.Shell.Command
import           HERMIT.Shell.Completion
import           HERMIT.Shell.Externals
import           HERMIT.Shell.Types hiding (clm)

import           HERMIT.GHCI.JSON
import           HERMIT.GHCI.Renderer
import           HERMIT.GHCI.Types

import           HERMIT.Shell.ShellEffect

import           HERMIT.Context

import           System.IO (Handle)

import           Debug.Trace

-- NOTES
--  * exprToDyns has useful info about building types

parseCLT :: (MonadIO m, Functor m) => Aeson.Value -> Maybe (CLT m Aeson.Value)
parseCLT = parseMaybe parseTopLevel
{-
  where topLevel =  alts
          [ parseTopLevel ShellEffectH
          , parseTopLevel (SetPathH      ::  TransformH LCoreTC LocalPathH -> TypedEffectH ())
          ]
-}          
parseTopLevel :: (MonadIO m, Functor m)
              => Aeson.Value -> Parser (CLT m Aeson.Value)
parseTopLevel v = fmap (const (toJSON ())) 
               <$> performTypedEffectH (show v) 
               <$> (parseClient v :: Parser (TypedEffectH ()))

-----------------------------------------------

--data ShellH where
--   ShellH :: (MonadIO m, Functor m) => CLT m Aeson.Value -> ShellH

instance External (TypedEffectH ())

instance FromJSON (Client (TypedEffectH ())) where
  parseJSON = alts 
    [ fmap (Client . ShellEffectH) . parseClient
    , external "setPath" (SetPathH :: TransformH LCoreTC LocalPathH -> TypedEffectH ())
    ]

-----------------------------------------------

instance FromJSON (Client (TransformH LCoreTC LocalPathH)) where
  parseJSON = alts 
    [ external "rhsOf" (rhsOfT . mkRhsOfPred :: RhsOfName -> TransformH LCoreTC LocalPathH)
    ]

-----------------------------------------------


instance FromJSON (Client ShellEffect) where
  parseJSON = alts
    [ external "display" $ CLSModify $ showWindowAlways Nothing
    , external "resume"  $ Resume
    ]

instance FromJSON (Client RhsOfName) where
  parseJSON (String txt) = return $ Client $ RhsOfName $ parseName $ Text.unpack $ txt
  parseJSON _ = fail "fail: RhsOfName"          
        
-----------------------------------------------
-- Utils

alts :: [a -> Parser b] -> a -> Parser b
alts as a = foldr (<|>) (fail "no match") $ map ($ a) as

method :: Text -> e -> Value -> Parser e
method nm e (Object o) = case parseMaybe (.: "method") o of
        Just nm' | nm' == nm -> return e
        _                    -> fail $ "no match for " ++ show nm

-----------------------------------------------

external :: External a => Text -> a -> Value -> Parser (Client (R a))
external nm f v | traceShow ("external",nm,v) False = undefined
external nm f v@(Object o) = case parseMaybe p o of
        Just (nm',args) | nm' == nm -> Client <$> parseExternal f args
        _                           -> fail $ "no match for " ++ show nm
 where p o = (,) <$> o .: "method"
                 <*> o .: "params"
 
class External e where
  type R e :: *
  type R e = e  -- default

  parseExternal :: e -> [Value] -> Parser (R e)

  default parseExternal :: e -> [Value] -> Parser e
  parseExternal e [] = return e
  parseExternal e _ = fail "wrong number of arguments"
  
instance (FromJSON (Client a), External b) => External (a -> b) where
  type R (a -> b) = R b
  parseExternal e (v:vs) = do
          a <- parseClient v
          parseExternal (e a) vs
          
  parseExternal e _ = fail "wrong number of arguments"

class Argument e where
  parseArgument :: Value -> Parser e

instance External ShellEffect 
instance External (TransformH a b)      -- a bit to open, I think

newtype Client a = Client a       -- name something better

parseClient :: FromJSON (Client e) => Value -> Parser e
parseClient v = (\ (Client e) -> e) <$> parseJSON v

