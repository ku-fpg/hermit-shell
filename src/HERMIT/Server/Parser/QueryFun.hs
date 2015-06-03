{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.QueryFun where

import           HERMIT.Kure
import           HERMIT.Shell.Types
import           HERMIT.Shell.Externals
import           HERMIT.Kernel (AST)

import           HERMIT.Server.Parser.Utils
import           HERMIT.Server.Parser.Transform()

import           Data.Aeson
import           Control.Monad

instance External QueryFun where
  parseExternals =
    [ fmap (QueryUnit :: TransformH LCore () -> QueryFun) . parseExternal
    , external "log"             (Inquiry showDerivationTree)
        [ "go back in the derivation" ]
    , external "diff"            Diff
        [ "show diff of two ASTs" ]
    , external "displayScripts" displayScripts
        ["Display all loaded scripts."]
    ]

instance External AST where
  parseExternal (Number n) = return $ (integerToAST . toInteger . floor) n
    where
      integerToAST = read . show
  parseExternal _ = mzero

