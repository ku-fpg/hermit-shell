{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Rewrite where

import           Data.Proxy

import           HERMIT.Dictionary
import           HERMIT.Kure
import           HERMIT.Lemma

import           HERMIT.Server.Parser.Name ()
import           HERMIT.Server.Parser.Utils

instance External (RewriteH LCore) where
  parseExternals =
    [ 
      -- HERMIT.API.Dictionary.AlphaConversion
      external "alpha" (promoteCoreR alphaR :: RewriteH LCore)
        [ "Renames the bound variables at the current node."]
    , external "alphaCase" (promoteExprR alphaCaseR :: RewriteH LCore)
        [ "Renames all binders in a Case alternative."]
    , external "alphaProg" (promoteProgR alphaProgR :: RewriteH LCore)
        [ "Rename all top-level identifiers in the program."]
    ,  external "unshadow" (promoteCoreR unshadowR :: RewriteH LCore)
        [ "Rename local variables with manifestly unique names (x, x0, x1, ...)."]
    
    , external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context
    , external "anyCall"   (const anyCallR_LCore :: Proxy LCore -> RewriteH LCore -> RewriteH LCore)
        [ "any-call (.. unfold command ..) applies an unfold command to all applications."
        , "Preference is given to applications with more arguments." ] .+ Deep
    ]
