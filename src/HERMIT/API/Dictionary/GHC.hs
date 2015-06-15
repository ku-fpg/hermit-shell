{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.GHC where

import Data.Aeson
import HERMIT.API.Types

-- | Deshadow a program.
deshadowProg :: Rewrite LCore
deshadowProg = Transform $ method "deshadowProg" []

-- | Zap the occurrence information in the current identifer if it is a zombie.
dezombify :: Rewrite LCore
dezombify = Transform $ method "dezombify" []

{-| 
  Perform dependency analysis on all sub-expressions; simplifying and updating 
  identifer info.
-}
occurrenceAnalysis :: Rewrite LCore
occurrenceAnalysis = Transform $ method "occurrenceAnalysis" []

{-| 
  Runs GHC's Core Lint, which typechecks the current expression.

  Note: this can miss several things that a whole-module core lint will find.
  For instance, running this on the RHS of a binding, the type of the RHS will
  not be checked against the type of the binding. 
  Running on the whole let expression will catch that however.
-}
lintExpr :: Transform LCoreTC String
lintExpr = Transform $ method "lintExpr" []

-- | Runs GHC's Core Lint, which typechecks the current module.
lintModule :: Transform LCoreTC String
lintModule = Transform $ method "lintModule" []

-- | Lint check a clause.
lint :: Transform LCoreTC String
lint = Transform $ method "lint" []

-- | Dynamically load a library of lemmas.
loadLemmaLibrary :: Name -> Transform LCore String
loadLemmaLibrary nm = Transform $ method "loadLemmaLibrary" 
    [toJSON nm, toJSON (Nothing :: Maybe Name)]

-- | Dynamically load a library of lemmas, specifying a specific lemma name.
loadLemmaLibraryWith :: Name -> Name -> Transform LCore String
loadLemmaLibraryWith nm lem = Transform $ method "loadLemmaLibrary" 
    [toJSON nm, toJSON (Just lem)]

-- | Inject a dependency on the given module.
injectDependency :: String -> Transform LCore ()
injectDependency str = Transform $ method "injectDependency" [toJSON str]
