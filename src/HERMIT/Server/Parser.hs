{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser where

import           Control.Monad.Reader

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe, Parser)

import           HERMIT.Context
import           HERMIT.Kure hiding ((<$>),(<*>))
import           HERMIT.Shell.Command
import           HERMIT.Shell.Types hiding (clm)
import           HERMIT.Shell.KernelEffect
import           HERMIT.Core (Crumb)

-- import           HERMIT.Context

import           HERMIT.Server.Parser.KernelEffect ()
import           HERMIT.Server.Parser.Name ()
import           HERMIT.Server.Parser.QueryFun ()
import           HERMIT.Server.Parser.ScriptEffect ()
import           HERMIT.Server.Parser.ShellEffect ()
import           HERMIT.Server.Parser.String ()
import           HERMIT.Server.Parser.Transform ()
import           HERMIT.Server.Parser.Utils
import           HERMIT.Server.Parser.Crumb()

import           Prelude.Compat

import           Control.Applicative ((<|>))

-- NOTES
--  * exprToDyns has useful info about building types

parseCLT :: (MonadIO m, Functor m) => Aeson.Value -> Maybe (CLT m Aeson.Value)
parseCLT = parseMaybe parseTopLevel

parseTopLevel :: (MonadIO m, Functor m)
              => Aeson.Value -> Parser (CLT m Aeson.Value)
parseTopLevel v = fmap (const (toJSON ()))
               <$> (   (performTypedEffectH (show v)
                          <$> (parseExternal v :: Parser (TypedEffectH ())))
                   <|> (performKernelEffect (stubExprH "<hermit-shell>")
                          <$> (parseExternal v :: Parser KernelEffect)))

instance External (TypedEffectH ()) where
  parseExternals =
    [ fmap ShellEffectH . parseExternal
--     , fmap RewriteLCoreH . parseExternal
    , fmap RewriteLCoreTCH . parseExternal
--     , fmap QueryH . parseExternal
--     , fmap SetPathH . (parseExternal :: Value -> Parser (TransformH LCoreTC LocalPathH))
--     , fmap SetPathH . (parseExternal :: Value -> Parser (TransformH LCore LocalPathH))
    , external "setPath" (SetPathH :: TransformH LCoreTC LocalPathH -> TypedEffectH ())
        ["sets the path"]
      -- XXX: Is this okay to do?
    , external "setPath" (SetPathH :: TransformH LCore LocalPathH -> TypedEffectH ())
        ["sets the path"]
    , external "setPath" ((SetPathH :: TransformH LCore LocalPathH -> TypedEffectH ()) . (\crumb -> transform (\ _hermitC _lcore -> return (singletonSnocPath crumb))) :: Crumb -> TypedEffectH ()) -- XXX: Is this how it should work?
        ["sets the path"]
     , external "query"   (QueryH :: QueryFun -> TypedEffectH ())
        ["performs query"]
    , external "rewrite" (RewriteLCoreH :: RewriteH LCore -> TypedEffectH ())
       ["performs query"]
    , external "eval" (EvalH :: String -> TypedEffectH ())
       ["performs legacy shell"]
    ]

