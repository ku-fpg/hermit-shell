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

instance External QueryFunBox where
  parseExternals =
    [ fmap (QueryFunBox . QueryUnit :: TransformH LCore () -> QueryFunBox) 
           parseExternal
    , external "log"
        (QueryFunBox $ Inquiry showDerivationTree)
    , external "diff"            
        (\ ast -> QueryFunBox . Diff ast)
    , external "displayScripts" 
        (QueryFunBox displayScripts)
    , fmap (fromAToBox . QueryA :: TransformH LCore String -> QueryFunBox) 
           parseExternal
    ]

fromAToBox :: QueryFun a -> QueryFunBox
fromAToBox x@QueryA{} = QueryFunBox x
fromAToBox _ = error "fromAToBox"

instance External AST where
  parsePrimitive (Number n) = return $ (integerToAST . fromInteger . floor) n
    where
      integerToAST :: Int -> AST
      integerToAST = read . show
  parsePrimitive _ = mzero

