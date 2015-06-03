{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Transform where


import           HERMIT.Context
import           HERMIT.Dictionary
import           HERMIT.GHC
import           HERMIT.Kure
import           HERMIT.Lemma
import           HERMIT.Name

import           HERMIT.Server.Parser.Name()
import           HERMIT.Server.Parser.Utils

instance External (TransformH LCoreTC LocalPathH) where
  parseExternals =
    [
      -- ???
      external "rhsOf"      (rhsOfT . mkRhsOfPred       :: RhsOfName -> TransformH LCoreTC LocalPathH)
            [ "Find the path to the RHS of the binding of the named variable." ]
    , external "bindingOf" (bindingOfT . mkBindingPred :: BindingName -> TransformH LCoreTC LocalPathH)
            [ "Find the path to the binding of the named variable." ]
    ]

instance External (TransformH LCoreTC String) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.GHC
      external "lintExpr" (promoteExprT lintExprT :: TransformH LCoreTC String)
        [ "Runs GHC's Core Lint, which typechecks the current expression."
        , "Note: this can miss several things that a whole-module core lint will find."
        , "For instance, running this on the RHS of a binding, the type of the RHS will"
        , "not be checked against the type of the binding. Running on the whole let expression"
        , "will catch that however."] .+ Deep .+ Debug .+ Query
    , external "lintModule" (promoteModGutsT lintModuleT :: TransformH LCoreTC String)
        [ "Runs GHC's Core Lint, which typechecks the current module."] .+ Deep .+ Debug .+ Query
    , external "lint" (promoteT lintClauseT :: TransformH LCoreTC String)
        [ "Lint check a clause." ]
    ]

instance External (TransformH LCore ()) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.GHC
      external "injectDependency" (promoteModGutsT . injectDependencyT . mkModuleName :: String -> TransformH LCore ())
        [ "Inject a dependency on the given module." ]

      -- ???
    , external "remember" (promoteCoreT . rememberR :: LemmaName -> TransformH LCore ()) -- Done not smell right (return ()?)
        [ "Remember the current binding, allowing it to be folded/unfolded in the future." ] .+ Context
    ]

-- instance External (TransformH LCore String) where
--   parseExternals =
--     [
--       -- HERMIT.API.Dictionary.GHC
--       external "loadLemmaLibrary" (flip loadLemmaLibraryT Nothing :: HermitName -> TransformH LCore String)
--         [ "Dynamically load a library of lemmas." ]
--     , external "loadLemmaLibrary" ((\nm -> loadLemmaLibraryT nm . Just) :: HermitName -> LemmaName -> TransformH LCore String)
--         [ "Dynamically load a specific lemma from a library of lemmas." ]
--     ]