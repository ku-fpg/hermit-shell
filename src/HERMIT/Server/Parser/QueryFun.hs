{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.QueryFun where

import           HERMIT.Kure
import           HERMIT.Shell.Types

import           HERMIT.Server.Parser.Utils
import           HERMIT.Server.Parser.Transform()

instance External QueryFun where
  parseExternals =
    [ fmap (QueryUnit :: TransformH LCore () -> QueryFun) . parseExternal ]
