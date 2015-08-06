{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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

instance External (QueryFun ()) where
  parseExternals =
    [ fmap (QueryUnit :: TransformH LCore () -> QueryFun ()) parseExternal
    , external "log"             (Inquiry showDerivationTree)
    , external "diff"            Diff
    , external "displayScripts" displayScripts
    ]

instance External AST where
  parsePrimitive (Number n) = return $ (integerToAST . fromInteger . floor) n
    where
      integerToAST :: Int -> AST
      integerToAST = read . show
  parsePrimitive _ = mzero

