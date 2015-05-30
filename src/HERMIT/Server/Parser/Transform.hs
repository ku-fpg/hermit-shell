{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Transform where


import           HERMIT.Context
import           HERMIT.Dictionary
import           HERMIT.Kure
import           HERMIT.Lemma
import           HERMIT.Name
import           HERMIT.Typeable()

import           HERMIT.Server.Parser.Name()
import           HERMIT.Server.Parser.Utils

instance External (TransformH LCoreTC LocalPathH) where
  parseExternals =
    [ external "rhsOf"      (rhsOfT . mkRhsOfPred       :: RhsOfName -> TransformH LCoreTC LocalPathH)
            [ "Find the path to the RHS of the binding of the named variable." ]
    , external "bindingOf" (bindingOfT . mkBindingPred :: BindingName -> TransformH LCoreTC LocalPathH)
            [ "Find the path to the binding of the named variable." ]
    ]

instance External (TransformH LCore ()) where
  parseExternals =
    [ external "remember" (promoteCoreT . rememberR :: LemmaName -> TransformH LCore ()) -- Done not smell right (return ()?)
        [ "Remember the current binding, allowing it to be folded/unfolded in the future." ] .+ Context
    ]
