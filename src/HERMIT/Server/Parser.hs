{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser where

import           Control.Monad.Reader

import           Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types (parseMaybe, Parser)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Typeable (Proxy(..))

import           HERMIT.Context
import           HERMIT.Kure hiding ((<$>),(<*>))
import           HERMIT.Shell.Command
import           HERMIT.Shell.Types hiding (clm)
import           HERMIT.Shell.KernelEffect
import           HERMIT.Shell.Proof
import           HERMIT.Core (Crumb)


import           HERMIT.Server.Parser.KernelEffect ()
import           HERMIT.Server.Parser.Name ()
import           HERMIT.Server.Parser.QueryFun ()
import           HERMIT.Server.Parser.ScriptEffect ()
import           HERMIT.Server.Parser.ShellEffect (parseExternalShellEffect)
import           HERMIT.Server.Parser.String ()
import           HERMIT.Server.Parser.Transform ()
import           HERMIT.Server.Parser.Utils
import           HERMIT.Server.Parser.Crumb ()
import           HERMIT.Server.Parser.ProofShellCommand ()

import           Prelude.Compat

import           Control.Applicative ((<|>))

import Debug.Trace

-- NOTES
--  * exprToDyns has useful info about building types

parseCLT :: (MonadIO m, Functor m) => Aeson.Value -> Maybe (CLT m Aeson.Value)
parseCLT = parseMaybe parseTopLevel

parseTopLevel :: (MonadIO m, Functor m)
              => Aeson.Value -> Parser (CLT m Aeson.Value)
parseTopLevel v = performTypedEffectH (pprint v) 
              <$> runExternalParser parseExternalTypedEffectH v
  where
    -- The Show instance for Value prints out Vector literals, which have
    -- different output depending on which version of vector is being used.
    -- This is inconvenient for diffing purposes, so we use a pretty-printer
    -- to make the output more consistent.
    pprint :: Aeson.Value -> String
    pprint = unpack . encodePretty


parseExternalTypedEffectH :: ExternalParser (TypedEffectH Value)
parseExternalTypedEffectH = 
       ShellEffectH <$> parseExternalShellEffect
   <|> parseToValue (Proxy :: Proxy (TypedEffectH ())) 

instance External (TypedEffectH ()) where
  parseExternals =
    [ fmap RewriteLCoreTCH parseExternal
    , external "setPath" (SetPathH :: TransformH LCoreTC LocalPathH -> TypedEffectH ())
    , external "setPath" ((SetPathH :: TransformH LCore LocalPathH -> TypedEffectH ()) . (\crumb -> transform (\ _hermitC _lcore -> return (singletonSnocPath crumb))) :: Crumb -> TypedEffectH ())
    , external "query"   (QueryH :: QueryFun () -> TypedEffectH ())
    , external "rewrite" (RewriteLCoreH :: RewriteH LCore -> TypedEffectH ())
    , external "eval" (EvalH :: String -> TypedEffectH ())
    ]


instance External TypedEffectBox where
  parseExternals =
    [ external "query"
        (fromBoxToBox :: QueryFunBox -> TypedEffectBox)
    , external "setPath"
        (TypedEffectBox . SetPathH :: TransformH LCore LocalPathH
                                   -> TypedEffectBox)
    ]

fromBoxToBox :: QueryFunBox -> TypedEffectBox
fromBoxToBox (QueryFunBox x) = TypedEffectBox $ QueryH x
