{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Transform where

import           HERMIT.Context
import           HERMIT.Dictionary
import           HERMIT.Kure
import           HERMIT.Name

import           HERMIT.Server.Parser.Name()
import           HERMIT.Server.Parser.Utils

instance External (TransformH LCoreTC LocalPathH) where
  parseExternal = alts 
    [ external "rhsOf"      (rhsOfT . mkRhsOfPred       :: RhsOfName -> TransformH LCoreTC LocalPathH)
            [ "Find the path to the RHS of the binding of the named variable." ]
--    , external "binding-of" (bindingOfT . mkBindingPred :: BindingName -> TransformH LCoreTC LocalPathH)
--            [ "Find the path to the binding of the named variable." ]
    ]
