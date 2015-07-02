{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-} -- TODO: until 7.10-only
{-# LANGUAGE StandaloneDeriving #-} -- TODO: until 7.10-only
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Transform () where

import                Control.Arrow
-- import                Control.Monad

-- import                Data.Proxy

import                HERMIT.Context
import                HERMIT.Dictionary
import                HERMIT.GHC
import                HERMIT.Kure
import                HERMIT.Lemma
import                HERMIT.Name
import                HERMIT.ParserCore

-- import                HERMIT.Server.Parser.Crumb ()
import                HERMIT.Server.Parser.Name ()
import {-# SOURCE #-} HERMIT.Server.Parser.Rewrite ()
import                HERMIT.Server.Parser.String ()
import                HERMIT.Server.Parser.Utils

import                Data.Typeable

import                Text.PrettyPrint.MarkedHughesPJ (MDoc) -- TODO: until 7.10

-------------------------------------------------------------------------------

instance External (TransformH LCore LocalPathH) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.Navigation
      external "consider"
        (considerConstructT :: Considerable -> TransformH LCore LocalPathH)
        [ "consider <c> focuses on the first construct <c>."
        , recognizedConsiderables ]

    , external "arg"
        (promoteExprT . nthArgPath :: Int -> TransformH LCore LocalPathH)
        [ "arg n focuses on the (n-1)th argument of a nested application." ]

    , external "lamsBody"
        (promoteExprT lamsBodyT :: TransformH LCore LocalPathH)
        [ "Descend into the body after a sequence of lambdas." ]

    , external "letsBody"
        (promoteExprT letsBodyT :: TransformH LCore LocalPathH)
        [ "Descend into the body after a sequence of let bindings." ]

    , external "progEnd"
        (promoteModGutsT gutsProgEndT <+
         promoteProgT progEndT :: TransformH LCore LocalPathH)
        [ "Descend to the end of a program." ]

    , external "parentOfCore"
        (parentOfT :: TransformH LCore LocalPathH
                   -> TransformH LCore LocalPathH)
        [ "Focus on the parent of another focal point." ]
    ]

instance External (TransformH LCoreTC LocalPathH) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.Navigation
      external "rhsOf"
        (rhsOfT . mkRhsOfPred :: RhsOfName -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the RHS of the binding of the named variable." ]

    , external "bindingGroupOf"
        (bindingGroupOfT .
         cmpString2Var :: String -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the binding group of the named variable." ]

    , external "bindingOf"
        (bindingOfT .
         mkBindingPred :: BindingName -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the binding of the named variable." ]

    , external "occurrenceOf"
        (occurrenceOfT .
         mkOccPred :: OccurrenceName -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the first occurrence of the named variable." ]

    , external "applicationOf"
        (applicationOfT .
         mkOccPred :: OccurrenceName -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the first application of the named variable." ]

    , external "parentOfCoreTC"
        (parentOfT :: TransformH LCoreTC LocalPathH
                   -> TransformH LCoreTC LocalPathH)
        [ "Focus on the parent of another focal point." ]
    ]

instance External (TransformH LCoreTC String) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.GHC
      external "lintExpr"
        (promoteExprT lintExprT :: TransformH LCoreTC String)
        [ "Runs GHC's Core Lint, which typechecks the current expression."
        , "Note: this can miss several things that a whole module core " ++
          "lint will find."
        , "For instance, running this on the RHS of a binding, the type of " ++
          "the RHS will"
        , "not be checked against the type of the binding. Running on the " ++
          "whole let expression"
        , "will catch that however."] .+ Deep .+ Debug .+ Query

    , external "lintModule"
        (promoteModGutsT lintModuleT :: TransformH LCoreTC String)
        [ "Runs GHC's Core Lint, which typechecks the current module."
        ] .+ Deep .+ Debug .+ Query

    , external "lint"
        (promoteT lintClauseT :: TransformH LCoreTC String)
        [ "Lint check a clause." ]

-- HERMIT.API.Dictionary.KURE
    , external "focus"
        (hfocusT :: TransformH LCoreTC LocalPathH -> TransformH LCoreTC String
                 -> TransformH LCoreTC String)
        [ "Apply a query at a focal point."] .+ Navigation .+ Deep

    , external "showRules" (rulesHelpListT :: TransformH LCoreTC String)
        [ "List all the rules in scope." ] .+ Query

-- HERMIT.API.Dictionary.Query
    , external "info"
        (promoteCoreTCT infoT :: TransformH LCoreTC String)
        [ "Display information about the current node." ] .+ Query
    ]

instance External (TransformH LCore ()) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.GHC
      external "injectDependency"
        (promoteModGutsT . injectDependencyT . mkModuleName
         :: String -> TransformH LCore ())
        [ "Inject a dependency on the given module." ]

-- HERMIT.API.Dictionary.KURE
    , external "success"
        (successT :: TransformH LCore ())
        [ "An always succeeding translation." ]

    , external "<+"
        ((<+) :: TransformH LCore () -> TransformH LCore ()
              -> TransformH LCore ())
        [ "Perform the first check, and then, if it fails, perform the " ++
          "second check." ]

    , external "not_"
       (notM :: TransformH LCore () -> TransformH LCore ())
       [ "Cause a failing check to succeed, a succeeding check to fail."
       ] .+ Predicate

-- HERMIT.API.Dictionary.New
    , external "var"
        (promoteExprT . isVar :: String -> TransformH LCore ())
        [ "var '<v> returns successfully for variable v, and fails otherwise."
        , "Useful in combination with \"when\", as in: when (var v) r"
        ] .+ Predicate

      -- HERMIT.API.Dictionary.Remembered
    , external "remember" (promoteCoreT . rememberR :: LemmaName -> TransformH LCore ()) -- Done not smell right (return ()?)
        [ "Remember the current binding, allowing it to be folded/unfolded in the future." ] .+ Context

    , external "wwResultGenerateFusion" (wwResultGenerateFusionT . mkWWAssC :: RewriteH LCore -> TransformH LCore ())
                   [ "Given a proof of Assumption C (fix (X->A) (\\ h x -> abs (rep (f h x))) ==> fix (X->A) f), then",
                     "execute this command on \"work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)\" to enable the \"wwResultFusion\" rule thereafter.",
                     "Note that this is performed automatically as part of \"wwResultSplit\"."
                   ] .+ Experiment .+ TODO
    , external "wwResultGenerateFusionUnsafe" (wwResultGenerateFusionT Nothing :: TransformH LCore ())
                   [ "Execute this command on \"work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)\" to enable the \"wwFusion\" rule thereafter.",
                     "The precondition \"fix (X->A) (\\ h x -> abs (rep (f h x))) == fix (X->A) f\" is expected to hold.",
                     "Note that this is performed automatically as part of \"wwResultSplit\"."
                   ] .+ Experiment .+ TODO
     , external "wwGenerateFusion" (wwGenerateFusionT . mkWWAssC :: RewriteH LCore -> TransformH LCore ())
                   [ "Given a proof of Assumption C (fix A (\\ a -> wrap (unwrap (f a))) ==> fix A f), then",
                     "execute this command on \"work = unwrap (f (wrap work))\" to enable the \"wwFusion\" rule thereafter.",
                     "Note that this is performed automatically as part of \"wwSplit\"."
                   ] .+ Experiment .+ TODO
    , external "wwGenerateFusionUnsafe" (wwGenerateFusionT Nothing :: TransformH LCore ())
                   [ "Execute this command on \"work = unwrap (f (wrap work))\" to enable the \"wwFusion\" rule thereafter.",
                     "The precondition \"fix A (wrap . unwrap . f) == fix A f\" is expected to hold.",
                     "Note that this is performed automatically as part of \"wwSplit\"."
                   ] .+ Experiment .+ TODO

   , external "introWWAssumptionA"
      (\nm absC repC -> do
            q <- parse2BeforeT assumptionAClauseT absC repC
            insertLemmaT nm $ Lemma q NotProven NotUsed :: TransformH LCore ())
        [ "Introduce a lemma for worker/wrapper assumption A"
        , "using given abs and rep functions." ]
   , external "introWWAssumptionB"
      (\nm absC repC bodyC -> do
            q <- parse3BeforeT assumptionBClauseT absC repC bodyC
            insertLemmaT nm $ Lemma q NotProven NotUsed :: TransformH LCore ())
        [ "Introduce a lemma for worker/wrapper assumption B"
        , "using given abs, rep, and body functions." ]
   , external "introWWAssumptionC"
      (\nm absC repC bodyC -> do
            q <- parse3BeforeT assumptionCClauseT absC repC bodyC
            insertLemmaT nm $ Lemma q NotProven NotUsed :: TransformH LCore ())
        [ "Introduce a lemma for worker/wrapper assumption C"
        , "using given abs, rep, and body functions." ]
    , external "isUndefinedVal" (promoteExprT isUndefinedValT :: TransformH LCore ())
        [ "Succeed if the current expression is an undefined value."
        ] .+ Shallow .+ Context .+ Predicate
    , external "imply" (\n1 n2 n3 -> implyLemmasT n1 n2 n3 :: TransformH LCore ())
        [ "imply new-name antecedent-name consequent-name" ]
    , external "instLemma" (\ nm v cs -> modifyLemmaT nm id (instantiateClauseVarR (cmpHN2Var v) cs) id id :: TransformH LCore ())
        [ "Instantiate one of the universally quantified variables of the given lemma,"
        , "with the given Core expression, creating a new lemma. Instantiating an"
        , "already proven lemma will result in the new lemma being considered proven." ]
   , external "copyLemma" (\ nm newName -> modifyLemmaT nm (const newName) idR id id :: TransformH LCore ())
        [ "Copy a given lemma, with a new name." ]
        , external "modifyLemma" ((\ nm rr -> modifyLemmaT nm id (extractR rr) (const NotProven) (const NotUsed)) :: LemmaName -> RewriteH LCore -> TransformH LCore ())
        [ "Modify a given lemma. Resets proven status to Not Proven and used status to Not Used." ]
    , external "conjunct" (\n1 n2 n3 -> conjunctLemmasT n1 n2 n3 :: TransformH LCore ())
        [ "conjunct new-name lhs-name rhs-name" ]
    , external "disjunct" (\n1 n2 n3 -> disjunctLemmasT n1 n2 n3 :: TransformH LCore ())
        [ "disjunct new-name lhs-name rhs-name" ]
   ]
    where
      mkWWAssC :: RewriteH LCore -> Maybe WWAssumption
      mkWWAssC r = Just (WWAssumption C (extractR r))

instance External (TransformH LCore String) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.GHC
      external "loadLemmaLibrary"
        (loadLemmaLibraryT :: HermitName -> Maybe LemmaName
                           -> TransformH LCore String)
        [ "Dynamically load a library of lemmas." ]

-- HERMIT.API.Dictionary.KURE
    , external "focus"
        (hfocusT :: TransformH LCore LocalPathH -> TransformH LCore String
                 -> TransformH LCore String)
        [ "Apply a query at a focal point."] .+ Navigation .+ Deep

    , external "test"
        (testQuery :: RewriteH LCore -> TransformH LCore String)
        [ "Determine if a rewrite could be successfully applied." ]

    , external "extractT"
        (extractT :: TransformH LCoreTC String -> TransformH LCore String)
        [ "Extract a TransformLCoreString from a TransformLCoreTCString" ]

    , external "queryLemma" ((\ nm t -> getLemmaByNameT nm >>> arr lemmaC >>> extractT t) :: LemmaName -> TransformH LCore String -> TransformH LCore String)
        [ "Apply a transformation to a lemma, returning the result." ]

    , external "lhs" (promoteClauseT . lhsT :: TransformH LCore String -> TransformH LCore String)
        [ "Apply a transformation to the LHS of a quantified clause." ]
    , external "rhs" (promoteClauseT . rhsT :: TransformH LCore String -> TransformH LCore String)
        [ "Apply a transformation to the RHS of a quantified clause." ]
    , external "both" ((\t -> do (r,s) <- promoteClauseT (bothT t); return (unlines [r,s])) :: TransformH LCore String -> TransformH LCore String)
        [ "Apply a transformation to both sides of a quantified clause." ]
    ]

{-
instance External (TransformH LCore DocH) where
  parseExternals =
    [ external "ruleToLemma" ((\pp nm -> ruleToLemmaT nm >> liftPrettyH (pOptions pp) (showLemmaT (fromString (show nm)) pp)) :: PrettyPrinter -> RuleName -> TransformH LCore DocH)
        [ "Create a lemma from a GHC RULE." ]
    ]
-}

{-
instance External (TransformH LCoreTC DocH) where
  parseExternals =
    [ external "showRule" (ruleHelpT :: PrettyPrinter -> RuleName -> TransformH LCoreTC DocH)
        [ "Display details on the named rule." ] .+ Query
    ]
-}

instance External (TransformH LCoreTC ()) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.Query
      external "compareBoundIds"
        (compareBoundIds :: HermitName -> HermitName -> TransformH LCoreTC ())
        [ "Compare the definitions of two in-scope identifiers for alpha " ++
          "equality."] .+ Query .+ Predicate

    , external "compareCoreAt"
        (compareCoreAtT :: TransformH LCoreTC LocalPathH
                        -> TransformH LCoreTC LocalPathH
                        -> TransformH LCoreTC ())
        [ "Compare the core fragments at the end of the given paths " ++
          "for alpha-equality."] .+ Query .+ Predicate

-- HERMIT.API.Shell.Externals
{-
    , external "dumpLemma" ((\pp nm fp r w -> getLemmaByNameT nm >>> liftPrettyH (pOptions pp) (ppLemmaT pp nm) >>> dumpT fp pp r w) :: PrettyPrinter -> LemmaName -> FilePath -> String -> Int -> TransformH LCoreTC ())
        [ "Dump named lemma to a file."
        , "dumpLemma <pretty-printer> <lemma-name> <filename> <renderer> <width>" ]
-}
    ]


------------------------------------------------------------------------------

deriving instance Typeable MDoc

{-
instance External (PrettyH LCore) where
  parseExternals =
    [ external "showLemma" ((flip showLemmaT) :: PrettyPrinter -> LemmaName -> PrettyH LCore)
        [ "Display a lemma." ]
    ]
-}

