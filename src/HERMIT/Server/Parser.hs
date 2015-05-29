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

parseTopLevel :: (MonadIO m, Functor m)
              => Aeson.Value -> Parser (CLT m Aeson.Value)
parseTopLevel v = fmap (const (toJSON ())) 
               <$> performTypedEffectH (show v) 
               <$> (parseExternal v :: Parser (TypedEffectH ()))

-----------------------------------------------

instance External (TypedEffectH ()) where
  parseExternal = alts 
    [ fmap ShellEffectH . parseExternal
    , external "setPath" (SetPathH :: TransformH LCoreTC LocalPathH -> TypedEffectH ())
    ]

-----------------------------------------------

instance External (TransformH LCoreTC LocalPathH) where
  parseExternal = alts 
    [ external "rhsOf" (rhsOfT . mkRhsOfPred :: RhsOfName -> TransformH LCoreTC LocalPathH)
    ]

-----------------------------------------------

instance External ShellEffect where
  parseExternal = alts 
    [ external "display" $ CLSModify $ showWindowAlways Nothing
    , external "resume"  $ Resume
    ]

instance External RhsOfName where
  parseExternal (String txt) = return $ RhsOfName $ parseName $ Text.unpack $ txt
  parseExternal _            = fail "fail: RhsOfName"          

-----------------------------------------------
-- Utils

alts :: [a -> Parser b] -> a -> Parser b
alts as a = foldr (<|>) (fail "no match") $ map ($ a) as

-----------------------------------------------

external :: External a => Text -> a -> Value -> Parser (R a)
external nm f v | traceShow ("external",nm,v) False = undefined
external nm f v@(Object o) = case parseMaybe p o of
        Just (nm',args) | nm' == nm -> matchExternal f args
        _                           -> fail $ "no match for " ++ show nm
 where p o = (,) <$> o .: "method"
                 <*> o .: "params"
 
class External e where
  type R e :: *
  type R e = e  -- default
  
  parseExternal :: Value -> Parser e

  matchExternal :: e -> [Value] -> Parser (R e)

  default matchExternal :: e -> [Value] -> Parser e
  matchExternal e [] = return e
  matchExternal e _ = fail "wrong number of arguments"
  
instance (External a, External b) => External (a -> b) where
  type R (a -> b) = R b
  parseExternal _ = error "can not parseExternal for function"
  matchExternal e (v:vs) = do
          a <- parseExternal v
          matchExternal (e a) vs
          
  matchExternal e _ = fail "wrong number of arguments"

