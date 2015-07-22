{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import           HERMIT.GHCI.JSON
import           HERMIT.Core (Crumb)

import           HERMIT.Server.Parser.Name ()
import           HERMIT.Server.Parser.QueryFun ()
import           HERMIT.Server.Parser.ScriptEffect ()
import           HERMIT.Server.Parser.ShellEffect (parseExternalShellEffect)
import           HERMIT.Server.Parser.String ()
import           HERMIT.Server.Parser.Transform ()
import           HERMIT.Server.Parser.Utils
import           HERMIT.Server.Parser.Crumb ()
import           HERMIT.Server.Parser.ProofShellCommand ()
import           HERMIT.Debug

import           Prelude.Compat

import           Control.Applicative ((<|>))

import Debug.Trace

-- NOTES
--  * exprToDyns has useful info about building types

parseCLT :: (MonadIO m, Functor m) => Aeson.Value -> Maybe (CLT m Aeson.Value)
parseCLT = parseMaybe parseTopLevel

parseTopLevel :: (MonadIO m, Functor m)
              => Aeson.Value -> Parser (CLT m Aeson.Value)
parseTopLevel v = performTypedEffectH' 
              <$> runExternalParser parseExternalTypedEffectH v
  where

    showEffectH :: TypedEffectH a -> String
    showEffectH (ShellEffectH          effect) = "ShellEffectH"
    showEffectH (RewriteLCoreH         rr    ) = "RewriteLCoreH"
    showEffectH (RewriteLCoreTCH       rr    ) = "RewriteLCoreTCH"
    showEffectH (SetPathH              tt    ) = "SetPathH"
    showEffectH (QueryH                q     ) = "QueryH"
    showEffectH (ProofShellCommandH    ps    ) = "ProofShellCommandH"
    showEffectH (KernelEffectH         k     ) = "KernelEffectH"
    showEffectH (EvalH                 e     ) = "EvalH"
    showEffectH (FmapTypedEffectH f    e     ) = "FmapTypedEffectH: " ++ showEffectH e


    performTypedEffectH' :: (MonadCatch m, CLMonad m) => TypedEffectH a -> m a
    performTypedEffectH' e = do
            when debug $ do
              liftIO $ putStrLn $ "performTypedEffectH: " 
              liftIO $ putStrLn $ pprintJSON v
              liftIO $ print $ showEffectH e
            performTypedEffectH (pprintJSON v) e
            
            


parseExternalTypedEffectH :: ExternalParser (TypedEffectH Value)
parseExternalTypedEffectH = 
       ShellEffectH <$> parseExternalShellEffect
   <|> parseToValue (Proxy :: Proxy (TypedEffectH ())) 

instance External (TypedEffectH ()) where
  parseExternals =
    [ fmap RewriteLCoreTCH parseExternal
    , fmap ProofShellCommandH parseExternal
    , external "setPath" (SetPathH :: TransformH LCore LocalPathH -> TypedEffectH ())
    , external "setPath" (SetPathH :: TransformH LCoreTC LocalPathH -> TypedEffectH ())
    , external "setPath" ((SetPathH :: TransformH LCore LocalPathH -> TypedEffectH ()) . (\crumb -> transform (\ _hermitC _lcore -> return (singletonSnocPath crumb))) :: Crumb -> TypedEffectH ())
    , external "query"   (QueryH :: QueryFun () -> TypedEffectH ())
    , external "rewrite" (RewriteLCoreH :: RewriteH LCore -> TypedEffectH ())
    , external "eval" (EvalH :: String -> TypedEffectH ())
    , external "up" (KernelEffectH $ Direction U)
    , external "top" (KernelEffectH $ Direction T)
    , external "beginScope" (KernelEffectH BeginScope)
    , external "endScope"    (KernelEffectH EndScope)
    ]
