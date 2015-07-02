{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Rewrite () where

import           Control.Arrow
import           Control.Monad

import           Data.Proxy

import           HERMIT.Context
import           HERMIT.Core (Crumb)
import           HERMIT.Dictionary
import           HERMIT.External (CoreString)
import           HERMIT.GHC
import           HERMIT.Kure
import           HERMIT.Lemma
import           HERMIT.Name
import           HERMIT.ParserCore

import           HERMIT.Server.Parser.BiRewrite ()
import           HERMIT.Server.Parser.Crumb ()
import           HERMIT.Server.Parser.Name ()
import           HERMIT.Server.Parser.String ()
import           HERMIT.Server.Parser.Transform ()
import           HERMIT.Server.Parser.Utils

import           Prelude hiding (abs)

-------------------------------------------------------------------------------

instance External (RewriteH LCore) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.AlphaConversion
      external "alpha"
        (promoteCoreR alphaR :: RewriteH LCore)
        [ "Renames the bound variables at the current node."]

    , external "alphaLam"
        (promoteExprR . alphaLamR :: Maybe String -> RewriteH LCore)
        [ "Renames the bound variable in a Lambda expression.  Optionally " ++
          "accepts a new name to use." ]

    , external "alphaCaseBinder"
        (promoteExprR . alphaCaseBinderR :: Maybe String -> RewriteH LCore)
        [ "Renames the binder in a Case expression. Optionally accepts a " ++
          "new name to use." ]

    , external "alphaAlt"
        (promoteAltR alphaAltR :: RewriteH LCore)
        [ "Renames all binders in a Case alternative."]

    , external "alphaAltWith"
        (promoteAltR . alphaAltWithR :: [String] -> RewriteH LCore)
        [ "Renames all binders in a Case alternative using the " ++
          "user-provided list of new names."]

    , external "alphaCase"
        (promoteExprR alphaCaseR :: RewriteH LCore)
        [ "Renames all binders in a Case alternative."]

    , external "alphaLetWith"
        (promoteExprR . alphaLetWithR :: [String] -> RewriteH LCore)
        [ "Renames the bound variables in a Let expression using a list " ++
          "of suggested names."]

    , external "alphaLet"
        (promoteExprR alphaLetR :: RewriteH LCore)
        [ "Renames the bound variables in a Let expression."]

    , external "alphaTopWith"
        (promoteProgR . alphaProgConsWithR :: [String] -> RewriteH LCore)
        [ "Renames the bound identifiers in the top-level binding group " ++
          "at the head of the program using a list of suggested names."]

    , external "alphaTop"
        (promoteProgR alphaProgConsR :: RewriteH LCore)
        [ "Renames the bound identifiers in the top-level binding at the " ++
          "head of the program."]

    , external "alphaProg"
        (promoteProgR alphaProgR :: RewriteH LCore)
        [ "Rename all topLevel identifiers in the program."]

    ,  external "unshadow"
        (promoteCoreR unshadowR :: RewriteH LCore)
        [ "Rename local variables with manifestly unique names " ++
          "(x, x0, x1, ...)."]

-- HERMIT.API.Dictionary.Composite
    , external "unfoldBasicCombinator"
        (promoteExprR unfoldBasicCombinatorR :: RewriteH LCore)
        [ "Unfold the current expression if it is one of the basic combinators:"
        , "($), (.), id, flip, const, fst, snd, curry, and uncurry." ]

    , external "simplify"
        (simplifyR :: RewriteH LCore)
        [ "innermost (unfoldBasicCombinator <+ betaReducePlus <+ " ++
          "safeLetSubst <+ caseReduce <+ letElim)" ]

    , external "bash"
        (bashR :: RewriteH LCore)
        bashHelp .+ Eval .+ Deep .+ Loop

    , external "smash"
        (smashR :: RewriteH LCore)
        smashHelp .+ Eval .+ Deep .+ Loop .+ Experiment

    , external "bashExtendedWith"
        (bashExtendedWithR :: [RewriteH LCore] -> RewriteH LCore)
        [ "Run \"bash\" extended with additional rewrites.",
          "Note: be sure that the new rewrite either fails or makes " ++
          "progress, else this may loop."
        ] .+ Eval .+ Deep .+ Loop

    , external "smashExtendedWith"
        (smashExtendedWithR :: [RewriteH LCore] -> RewriteH LCore)
        [ "Run \"smash\" extended with additional rewrites.",
          "Note: be sure that the new rewrite either fails or makes " ++
          "progress, else this may loop."
        ] .+ Eval .+ Deep .+ Loop

    , external "bashDebug"
        (bashDebugR :: RewriteH LCore)
        [ "verbose bash - most useful with setAutoCorelint True"
        ] .+ Eval .+ Deep .+ Loop

-- HERMIT.API.Dictionary.FixPoint
    , external "fixIntro"
        (promoteCoreR fixIntroR :: RewriteH LCore)
        [ "rewrite a function binding into a nonRecursive binding using fix"
        ] .+ Introduce .+ Context

-- HERMIT.API.Dictionary.Fold
    , external "fold"
        (promoteExprR . foldR :: HermitName -> RewriteH LCore)
        [ "fold a definition"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        , ""
        , "5 + 5 + 6"
        , "anyBu (fold 'double)"
        , "double 5 + 6"
        , ""
        , "Note: due to associativity, if you wanted to fold 5 + 6 + 6, "
        , "you first need to apply an associativity rewrite."
        ]  .+ Context .+ Deep

-- HERMIT.API.Dictionary.Function
    , external "staticArg"
        (promoteDefR staticArgR :: RewriteH LCore)
        [ "perform the static argument transformation on a recursive function."
        ]

    , external "staticArgTypes"
        (promoteDefR staticArgTypesR :: RewriteH LCore)
        [ "perform the static argument transformation on a recursive " ++
          "function, only transforming type arguments." ]

    , external "staticArgPos"
        (promoteDefR . staticArgPosR :: [Int] -> RewriteH LCore)
        [ "perform the static argument transformation on a recursive " ++
          "function, only transforming the arguments specified (by index)." ]

-- HERMIT.API.Dictionary.GHC
    , external "deshadowProg"
        (promoteProgR deShadowProgR :: RewriteH LCore)
        [ "Deshadow a program." ] .+ Deep

    , external "dezombify"
        (promoteExprR dezombifyR :: RewriteH LCore)
        [ "Zap the occurrence information in the current identifer if it " ++
          "is a zombie."] .+ Shallow

    , external "occurrenceAnalysis"
        (occurrenceAnalysisR :: RewriteH LCore)
        [ "Perform dependency analysis on all subExpressions; simplifying " ++
          "and updating identifer info."] .+ Deep

-- HERMIT.API.Dictionary.Induction
    , external "caseSplitOn"
        ((\ fl -> promoteClauseR . caseSplitOnR fl . cmpHN2Var)
         :: Bool -> HermitName -> RewriteH LCore)
        [ "Case split or induct on specified value quantifier." ]

-- HERMIT.API.Dictionary.Inline
    , external "inline"
        (promoteExprR inlineR :: RewriteH LCore)
        [ "(Var v) ==> <defn of v>" ].+ Eval .+ Deep

    , external "inlineWith"
        (promoteExprR . inlineMatchingPredR . cmpHN2Var :: HermitName
                                                        -> RewriteH LCore)
        [ "Given a specific v, (Var v) ==> <defn of v>" ] .+ Eval .+ Deep

    , external "inlineAny"
        (promoteExprR . inlineNamesR :: [String] -> RewriteH LCore)
        [ "If the current variable matches any of the given names, then " ++
          "inline it." ] .+ Eval .+ Deep

    , external "inlineCaseScrutinee"
        (promoteExprR inlineCaseScrutineeR :: RewriteH LCore)
        [ "if v is a case binder, replace (Var v) with the bound case " ++
          "scrutinee." ] .+ Eval .+ Deep

    , external "inlineCaseAlternative"
        (promoteExprR inlineCaseAlternativeR :: RewriteH LCore)
        [ "if v is a case binder, replace (Var v) with the bound " ++
          "caseAlternative pattern." ] .+ Eval .+ Deep

-- HERMIT.API.Dictionary.KURE
    , external "idCore"
        (idR :: RewriteH LCore)
        [ "Perform an identity rewrite."] .+ Shallow

    , external "fail_"
        (fail :: String -> RewriteH LCore)
        [ "A failing rewrite."]

    , external "<+"
        ((<+) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)
        [ "Perform the first rewrite, and then, if it fails, perform " ++
          "the second rewrite." ]

    , external ">>>"
        ((>>>) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)
        [ "Compose rewrites, requiring both to succeed." ]

    , external ">+>"
        ((>+>) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)
        [ "Compose rewrites, allowing one to fail." ]

    , external "try"
        (tryR :: RewriteH LCore -> RewriteH LCore)
        [ "Try a rewrite, and perform the identity if the rewrite fails." ]

    , external "repeat"
        (repeatR :: RewriteH LCore -> RewriteH LCore)
        [ "Repeat a rewrite until it would fail." ] .+ Loop

    , external "replicate"
        ((\ n -> andR . replicate n) :: Int -> RewriteH LCore -> RewriteH LCore)
        [ "Repeat a rewrite n times." ]

    , external "all"
        (allR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to all children of the node, requiring success " ++
          "at every child." ] .+ Shallow

    , external "any"
        (anyR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to all children of the node, requiring success " ++
          "for at least one child." ] .+ Shallow

    , external "one"
        (oneR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the first child of the node for which it can " ++
          "succeed." ] .+ Shallow

    , external "allBU"
        (allbuR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in bottomUp " ++
          "order, requiring success at every node." ] .+ Deep

    , external "allTD"
        (alltdR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in top-down " ++
          "order, requiring success at every node." ] .+ Deep

    , external "allDU"
        (allduR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite twice, in a top-down and bottomUp way, using " ++
          "one single tree traversal,"
        , "succeeding if they all succeed."] .+ Deep

    , external "anyBU"
        (anybuR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in bottomUp " ++
          "order, requiring success for at least one node." ] .+ Deep

    , external "anyTD"
        (anytdR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in top-down " ++
          "order, requiring success for at least one node." ] .+ Deep

    , external "anyDU"
        (anyduR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite twice, in a top-down and bottomUp way, using " ++
          "one single tree traversal,"
        , "succeeding if any succeed."] .+ Deep

    , external "oneTD"
        (onetdR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the first node (in a top-down order) for " ++
          "which it can succeed." ] .+ Deep

    , external "oneBU"
        (onebuR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the first node (in a bottomUp order) for " ++
          "which it can succeed." ] .+ Deep

    , external "pruneTD"
        (prunetdR :: RewriteH LCore -> RewriteH LCore)
        [ "Attempt to apply a rewrite in a top-down manner, prunning at " ++
          "successful rewrites." ] .+ Deep

    , external "innermost"
        (innermostR :: RewriteH LCore -> RewriteH LCore)
        [ "A fixedPoint traveral, starting with the innermost term."
        ] .+ Deep .+ Loop

    , external "focus"
        (hfocusR :: TransformH LCore LocalPathH -> RewriteH LCore
                 -> RewriteH LCore)
        [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep

    , external "when"
        ((>>) :: TransformH LCore () -> RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite only if the check succeeds." ] .+ Predicate

    , external "forward"
        (forwardT :: BiRewriteH LCore -> RewriteH LCore)
        [ "Apply a bidirectional rewrite forewards." ]

    , external "backward"
        (backwardT :: BiRewriteH LCore -> RewriteH LCore)
        [ "Apply a bidirectional rewrite backwards." ]

    , external "anyCall"
        (const anyCallR_LCore :: Proxy LCore -> RewriteH LCore
                              -> RewriteH LCore)
        [ "anyCall (.. unfold command ..) applies an unfold command to " ++
          "all applications."
        , "Preference is given to applications with more arguments." ] .+ Deep

    , external "extractR"
        (extractR :: RewriteH LCoreTC -> RewriteH LCore)
        [ "Extract a RewriteCore from a RewriteCoreTC" ]

    , external "atPath"
        (flip hfocusT idR :: TransformH LCore LocalPathH
                          -> TransformH LCore LCore)
        [ "return the expression found at the given path" ]

   , external "atPathProj"
       (extractT . flip hfocusT projectT :: TransformH LCoreTC LocalPathH
                                         -> TransformH LCore LCore)
       [ "return the expression found at the given path" ]

      -- HERMIT.API.Dictionary.Local
    , external "betaReduce" (promoteExprR betaReduceR :: RewriteH LCore)
        [ "((\\ v -> E1) E2) ==> let v = E2 in E1"
        , "This form of beta-reduction is safe if E2 is an arbitrary expression"
        , "(won't duplicate work)." ]                                 .+ Eval .+ Shallow
    , external "betaExpand" (promoteExprR betaExpandR :: RewriteH LCore)
        [ "(let v = e1 in e2) ==> (\\ v -> e2) e1" ]                            .+ Shallow
    , external "etaReduce" (promoteExprR etaReduceR :: RewriteH LCore)
        [ "(\\ v -> e1 v) ==> e1" ]                                             .+ Eval .+ Shallow
    , external "etaExpand" (promoteExprR . etaExpandR :: String -> RewriteH LCore)
        [ "\"eta-expand 'v\" performs e1 ==> (\\ v -> e1 v)" ]                  .+ Shallow .+ Introduce
    , external "flattenModule" (promoteModGutsR flattenModuleR :: RewriteH LCore)
        [ "Flatten all the top-level binding groups in the module to a single recursive binding group."
        , "This can be useful if you intend to appply GHC RULES." ]
    , external "flattenProgram" (promoteProgR flattenProgramR :: RewriteH LCore)
        [ "Flatten all the top-level binding groups in a program (list of binding groups) to a single"
        , "recursive binding group.  This can be useful if you intend to apply GHC RULES." ]
    , external "abstract" (promoteExprR . abstractR . mkOccPred :: OccurrenceName -> RewriteH LCore)
        [ "Abstract over a variable using a lambda."
        , "e  ==>  (\\ x -> e) x" ]                                             .+ Shallow .+ Introduce .+ Context
    , external "push" ((\ nm strictf -> push (Just strictf) (cmpString2Var nm)) :: String -> RewriteH LCore -> RewriteH LCore)
        [ "Push a function 'f into a case-expression or let-expression argument,"
        , "given a proof that f (fully saturated with type arguments) is strict." ] .+ Shallow .+ Commute
    , external "pushUnsafe" (push Nothing . cmpString2Var :: String -> RewriteH LCore)
        [ "Push a function 'f into a case-expression or let-expression argument."
        , "Requires 'f to be strict." ] .+ Shallow .+ Commute .+ PreCondition .+ Unsafe

-- HERMIT.API.Dictionary.Local.Bind
    , external "nonrecToRec"
        (promoteBindR nonrecToRecR :: RewriteH LCore)
        [ "Convert a nonRecursive binding into a recursive binding group " ++
          "with a single definition."
        , "NonRec v e ==> Rec [Def v e]" ] .+ Shallow

    , external "recToNonrec"
        (promoteBindR recToNonrecR :: RewriteH LCore)
        [ "Convert a singleton recursive binding into a nonRecursive " ++
          "binding group."
        , "Rec [Def v e] ==> NonRec v e,  (v not free in e)" ]

-- HERMIT.API.Dictionary.Local.Case
    , external "caseFloatApp"
        (promoteExprR caseFloatAppR :: RewriteH LCore)
        [ "(case ec of alt -> e) v ==> case ec of alt -> e v"
        ] .+ Commute .+ Shallow

    , external "caseFloatArg"
        ((\ x -> promoteExprR . caseFloatArg x)
         :: Maybe CoreString -> Maybe (RewriteH LCore) -> RewriteH LCore)
        [ "Given a proof that f is strict, then"
        , "f (case s of alt -> e) ==> case s of alt -> f e"
        ] .+ Commute .+ Shallow

    , external "caseFloatArgUnsafe"
        ((\ x -> promoteExprR . caseFloatArgLemmaR x)
          :: Used -> LemmaName -> RewriteH LCore)
        [ "f (case s of alt -> e) ==> case s of alt -> f e"
        ] .+ Commute .+ Shallow .+ PreCondition .+ Strictness

    , external "caseFloatCase"
        (promoteExprR caseFloatCaseR :: RewriteH LCore)
        [ "case (case ec of alt1 -> e1) of alta -> ea ==> " ++
          "case ec of alt1 -> case e1 of alta -> ea" ] .+ Commute .+ Eval

    , external "caseFloatCast"
        (promoteExprR caseFloatCastR :: RewriteH LCore)
        [ "cast (case s of p -> e) co ==> case s of p -> cast e co"
        ] .+ Shallow .+ Commute

    , external "caseFloatLet"
        (promoteExprR caseFloatLetR :: RewriteH LCore)
        [ "let v = case ec of alt1 -> e1 in e ==> " ++
          "case ec of alt1 -> let v = e1 in e"
        ] .+ Commute .+ Shallow .+ Strictness

    , external "caseFloat"
        (promoteExprR caseFloatR :: RewriteH LCore)
        [ "caseFloat = caseFloatApp <+ caseFloatCase <+ caseFloatLet <+ " ++
          "caseFloatCase" ] .+ Commute .+ Shallow .+ Strictness

    , external "caseFloatIn"
        (promoteExprR caseFloatInR :: RewriteH LCore)
        [ "Float in a Case whatever the context."
        ] .+ Commute .+ Shallow .+ PreCondition

    , external "caseFloatInArgs"
        (promoteExprR caseFloatInArgsR :: RewriteH LCore)
        [ "Float in a Case whose alternatives are parallel applications " ++
          "of the same function."
        ] .+ Commute .+ Shallow .+ PreCondition .+ Strictness

    , external "caseReduce"
        (promoteExprR (caseReduceR True) :: RewriteH LCore)
        [ "Case of Known Constructor"
        , "caseReduceDatacon <+ caseReduceLiteral" ] .+ Shallow .+ Eval

    , external "caseReduceDatacon"
        (promoteExprR (caseReduceDataconR True) :: RewriteH LCore)
        [ "Case of Known Constructor"
        , "case C v1..vn of C w1..wn -> e ==> " ++
          "let { w1 = v1 ; .. ; wn = vn } in e" ] .+ Shallow .+ Eval

    , external "caseReduceLiteral"
        (promoteExprR (caseReduceLiteralR True) :: RewriteH LCore)
        [ "Case of Known Constructor"
        , "case L of L -> e ==> e" ] .+ Shallow .+ Eval

    , external "caseReduceUnfold"
        (promoteExprR (caseReduceUnfoldR True) :: RewriteH LCore)
        [ "Unfold the case scrutinee and then caseReduce."
        ] .+ Shallow .+ Eval .+ Context

    , external "caseSplit"
        ((\nm -> findVarT (unOccurrenceName nm) >>= promoteExprR . caseSplitR .
                 varToCoreExpr) :: OccurrenceName -> RewriteH LCore)
        [ "caseSplit 'x"
        , "e ==> case x of C1 vs -> e; C2 vs -> e, where x is free in e"
        ] .+ Deep .+ Strictness

    , external "caseSplitQQ"
        (parseCoreExprT >=> promoteR .
         caseSplitR :: CoreString -> RewriteH LCore)
        [ "caseSplit [| expr |]"
        , "e ==> case expr of C1 vs -> e; C2 vs -> e"] .+ Deep .+ Strictness

    , external "caseSplitInline"
        ((\nm -> findVarT (unOccurrenceName nm) >>= promoteExprR .
                 caseSplitInlineR .
                 varToCoreExpr) :: OccurrenceName -> RewriteH LCore)
        [ "Like caseSplit, but additionally inlines the matched constructor "
        , "applications for all occurances of the named variable."
        ] .+ Deep .+ Strictness

    , external "caseSplitInlineQQ"
        (parseCoreExprT >=> promoteExprR .
         caseSplitInlineR :: CoreString -> RewriteH LCore)
        [ "Like caseSplit, but additionally inlines the matched constructor "
        , "applications for all occurances of the case binder."
        ] .+ Deep .+ Strictness

    , external "caseIntroSeq"
        (promoteExprR . caseIntroSeqR .
         cmpString2Var :: String -> RewriteH LCore)
        [ "Force evaluation of a variable by introducing a case."
        , "caseIntroSeq 'v is is equivalent to adding @(seq v)@ in the " ++
          "source code." ] .+ Shallow .+ Introduce .+ Strictness

    , external "caseElimSeq"
        (promoteExprR caseElimSeqR :: RewriteH LCore)
        [ "Eliminate a case that corresponds to a pointless seq."
        ] .+ Deep .+ Eval .+ Strictness

    , external "caseInlineAlternative"
        (promoteExprR caseInlineAlternativeR :: RewriteH LCore)
        [ "Inline the case binder as the caseAlternative pattern " ++
          "everywhere in the case alternatives." ] .+ Deep

    , external "caseInlineScrutinee"
        (promoteExprR caseInlineScrutineeR :: RewriteH LCore)
        [ "Inline the case binder as the case scrutinee everywhere in " ++
          "the case alternatives." ] .+ Deep

    , external "caseMergeAlts"
        (promoteExprR caseMergeAltsR :: RewriteH LCore)
        [ "Merge all case alternatives into a single default case."
        , "The RHS of each alternative must be the same."
        , "case s of {pat1 -> e ; pat2 -> e ; ... ; patn -> e} ==> " ++
          "case s of {_ -> e}" ]

    , external "caseMergeAltsWithBinder"
        (promoteExprR caseMergeAltsWithBinderR :: RewriteH LCore)
        [ "A cleverer version of 'mergeCaseAlts' that first attempts to"
        , "abstract out any occurrences of the alternative pattern using " ++
          "the case binder." ] .+ Deep

    , external "caseElim"
        (promoteExprR caseElimR :: RewriteH LCore)
        [ "case s of w; C vs -> e ==> e if w and vs are not free in e"
        ] .+ Shallow .+ Strictness

    , external "caseElimInlineScrutinee"
        (promoteExprR caseElimInlineScrutineeR :: RewriteH LCore)
        [ "Eliminate a case, inlining any occurrences of the case binder " ++
          "as the scrutinee." ] .+ Deep

    , external "caseElimMergeAlts"
        (promoteExprR caseElimMergeAltsR :: RewriteH LCore)
        [ "Eliminate a case, merging the case alternatives into a single " ++
          "default alternative",
          "and inlining the case binder as the scrutinee (if possible)."
        ] .+ Deep

    , external "caseFoldBinder"
        (promoteExprR caseFoldBinderR :: RewriteH LCore)
        [ "In the case alternatives, fold any occurrences of the case alt " ++
          "patterns to the case binder." ]

-- HERMIT.API.Dictionary.Local.Cast
    , external "castElim"
        (promoteExprR castElimR :: RewriteH LCore)
        [ "castElimRefl <+ castElimSym" ] .+ Shallow

    , external "castElimRefl"
        (promoteExprR castElimReflR :: RewriteH LCore)
        [ "cast e co ==> e ; if co is a reflexive coercion" ] .+ Shallow

    , external "castElimSym"
        (promoteExprR castElimSymR :: RewriteH LCore)
        [ "removes pairs of symmetric casts" ] .+ Shallow

    , external "castElimSymPlus"
        (promoteExprR castElimSymPlusR :: RewriteH LCore)
        [ "removes pairs of symmetric casts possibly separated by let " ++
          "or case forms" ] .+ Deep .+ TODO

    , external "castFloatApp"
        (promoteExprR castFloatAppR :: RewriteH LCore)
        [ "(cast e (c1 -> c2)) x ==> cast (e (cast x (sym c1))) c2" ] .+ Shallow

    , external "castFloatLam"
        (promoteExprR castFloatLamR :: RewriteH LCore)
        [ "\\ x::a -> cast x (a -> b) ==> " ++
          "cast (\\x::a -> x) ((a -> a) -> (a -> b))" ] .+ Shallow

    , external "castElimUnsafe"
        (promoteExprR castElimUnsafeR :: RewriteH LCore)
        [ "removes casts regardless of whether it is safe to do so"
        ] .+ Shallow .+ Experiment .+ Unsafe .+ TODO

-- HERMIT.API.Dictionary.Local.Let
    , external "letSubst"
        (promoteExprR letSubstR :: RewriteH LCore)
        [ "Let substitution: (let x = e1 in e2) ==> (e2[e1/x])"
        , "x must not be free in e1." ] .+ Deep .+ Eval

    , external "letSubstSafe"
        (promoteExprR letSubstSafeR :: RewriteH LCore)
        [ "Safe let substitution"
        , "let x = e1 in e2, safe to inline without duplicating work ==> " ++
          "e2[e1/x],"
        , "x must not be free in e1." ] .+ Deep .+ Eval

    , external "letNonrecSubstSafe"
        (promoteExprR letNonRecSubstSafeR :: RewriteH LCore)
        [ "As letSubstSafe, but does not try to convert a recursive let " ++
          "into a nonRecursive let first." ] .+ Deep .+ Eval

    , external "letIntro"
        (promoteExprR . letIntroR :: String -> RewriteH LCore)
        [ "e => (let v = e in v), name of v is provided"
        ] .+ Shallow .+ Introduce

    , external "letIntroUnfolding"
        (promoteExprR . letIntroUnfoldingR :: HermitName -> RewriteH LCore)
        [ "e => let f' = defn[f'/f] in e[f'/f], name of f is provided" ]

    , external "letElim"
        (promoteExprR letElimR :: RewriteH LCore)
        [ "Remove an unused let binding."
        , "(let v = e1 in e2) ==> e2, if v is not free in e1 or e2."
        ] .+ Eval .+ Shallow

    , external "letFloatApp"
        (promoteExprR letFloatAppR :: RewriteH LCore)
        [ "(let v = ev in e) x ==> let v = ev in e x" ] .+ Commute .+ Shallow

    , external "letFloatArg"
        (promoteExprR letFloatArgR :: RewriteH LCore)
        [ "f (let v = ev in e) ==> let v = ev in f e" ] .+ Commute .+ Shallow

    , external "letFloatLam"
        (promoteExprR letFloatLamR :: RewriteH LCore)
        [ "The Full Laziness Transformation"
        , "(\\ v1 -> let v2 = e1 in e2)  ==>  let v2 = e1 in (\\ v1 -> e2), " ++
          "if v1 is not free in e2."
        , "If v1 = v2 then v1 will be alphaRenamed." ] .+ Commute .+ Shallow

    , external "letFloatLet"
        (promoteExprR letFloatLetR :: RewriteH LCore)
        [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e"
        ] .+ Commute .+ Shallow

    , external "letFloatCase"
        (promoteExprR letFloatCaseR :: RewriteH LCore)
        [ "case (let v = ev in e) of ... ==> let v = ev in case e of ..."
        ] .+ Commute .+ Shallow .+ Eval

    , external "letFloatCaseAlt"
        (promoteExprR . letFloatCaseAltR :: Maybe Int -> RewriteH LCore)
        [ "case s of { ... ; p -> let v = ev in e ; ... } "
        , "==> let v = ev in case s of { ... ; p -> e ; ... } "
        ] .+ Commute .+ Shallow .+ Eval

    , external "letFloatCast"
        (promoteExprR letFloatCastR :: RewriteH LCore)
        [ "cast (let bnds in e) co ==> let bnds in cast e co"
        ] .+ Commute .+ Shallow

    , external "letFloatTop"
        (promoteProgR letFloatTopR :: RewriteH LCore)
        [ "v = (let bds in e) : prog ==> bds : v = e : prog"
        ] .+ Commute .+ Shallow

    , external "letFloat"
        (promoteProgR letFloatTopR <+
         promoteExprR letFloatExprR :: RewriteH LCore)
        [ "Float a Let whatever the context." ] .+ Commute .+ Shallow

    , external "letToCase"
        (promoteExprR letToCaseR :: RewriteH LCore)
        [ "let v = ev in e ==> case ev of v -> e"
        ] .+ Commute .+ Shallow .+ PreCondition

    , external "letFloatIn"
        (promoteExprR letFloatInR >+>
         anybuR (promoteExprR letElimR) :: RewriteH LCore)
        [ "FloatIn a let if possible." ] .+ Commute .+ Shallow

    , external "letFloatInApp"
        ((promoteExprR letFloatInAppR >+>
          anybuR (promoteExprR letElimR)) :: RewriteH LCore)
        [ "let v = ev in f a ==> (let v = ev in f) (let v = ev in a)"
        ] .+ Commute .+ Shallow

    , external "letFloatInCase"
        ((promoteExprR letFloatInCaseR >+>
          anybuR (promoteExprR letElimR)) :: RewriteH LCore)
        [ "let v = ev in case s of p -> e ==> " ++
          "case (let v = ev in s) of p -> let v = ev in e"
        , "if v does not shadow a pattern binder in p" ] .+ Commute .+ Shallow

    , external "letFloatInLam"
        ((promoteExprR letFloatInLamR >+>
          anybuR (promoteExprR letElimR)) :: RewriteH LCore)
        [ "let v = ev in \\ x -> e ==> \\ x -> let v = ev in e"
        , "if v does not shadow x" ] .+ Commute .+ Shallow

     , external "reorderLets"
         (promoteExprR . reorderNonRecLetsR :: [String] -> RewriteH LCore)
         [ "Reorder a sequence of nested nonRecursive let bindings."
         , "The argument list should contain the letBound variables, in " ++
           "the desired order." ]

    , external "letTuple"
        (promoteExprR . letTupleR :: String -> RewriteH LCore)
        [ "Combine nested nonRecursive lets into case of a tuple."
        , "E.g. let {v1 = e1 ; v2 = e2 ; v3 = e3} in body ==> " ++
          "case (e1,e2,e3) of {(v1,v2,v3) -> body}" ] .+ Commute

    , external "progBindElim"
        (promoteProgR progBindElimR :: RewriteH LCore)
        [ "Remove unused topLevel binding(s)."
        , "progBindNonrecElim <+ progBindRecElim" ] .+ Eval .+ Shallow

    , external "progBindNonrecElim"
        (promoteProgR progBindNonRecElimR :: RewriteH LCore)
        [ "Remove unused topLevel binding(s)."
        , "v = e : prog ==> prog, if v is not free in prog and not exported."
        ] .+ Eval .+ Shallow

    , external "progBindRecElim"
        (promoteProgR progBindRecElimR :: RewriteH LCore)
        [ "Remove unused topLevel binding(s)."
        , "v+ = e+ : prog ==> v* = e* : prog, where v* is a subset of " ++
          "v+ consisting"
        , "of vs that are free in prog or e+, or exported." ] .+ Eval .+ Shallow

-- HERMIT.API.Dictionary.New
    , external "nonrecIntro"
        ((\ s str -> promoteCoreR (nonRecIntro s str))
         :: String -> CoreString -> RewriteH LCore)
        [ "Introduce a new non-recursive binding.  Only works at " ++
          "Expression or Program nodes."
        , "nonrec-into 'v [| e |]"
        , "body ==> let v = e in body"
        ] .+ Introduce .+ Shallow

      -- HERMIT.API.Dictionary.Remembered
    , external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context

    , external "foldRemembered" (promoteExprR . foldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Fold a remembered definition." ]                      .+ Context .+ Deep

    , external "foldAnyRemembered" (promoteExprR foldAnyRememberedR :: RewriteH LCore)
        [ "Attempt to fold any of the remembered definitions." ] .+ Context .+ Deep

    , external "wwResultSplit" ((\ abs rep assC -> promoteDefR $ wwResultSplit (mkWWAssC assC) abs rep)
                                  :: CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
                [ "Worker/Wrapper Split (Result Variant)",
                  "For any \"prog :: X -> A\", and given \"abs :: B -> A\" and \"rep :: A -> B\" as arguments,",
                  "and a proof of Assumption C (fix (X->A) (\\ h x -> abs (rep (f h x))) ==> fix (X->A) f), then",
                  "prog = expr  ==>  prog = let f = \\ prog -> expr",
                  "                          in let work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)",
                  "                              in \\ x0 -> abs (work x0)"
                ] .+ Introduce .+ Context
    , external "wwResultSplitUnsafe" ((\ abs rep -> promoteDefR $ wwResultSplit Nothing abs rep)
                                       :: CoreString -> CoreString -> RewriteH LCore)
                [ "Unsafe Worker/Wrapper Split (Result Variant)",
                  "For any \"prog :: X -> A\", and given \"abs :: B -> A\" and \"rep :: A -> B\" as arguments, then",
                  "prog = expr  ==>  prog = let f = \\ prog -> expr",
                  "                          in let work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)",
                  "                              in \\ x0 -> abs (work x0)",
                  "Note: the preCondition \"fix (X->A) (\\ h x -> abs (rep (f h x))) == fix (X->A) f\" is expected to hold."
                ] .+ Introduce .+ Context .+ PreCondition .+ Unsafe
    , external "wwResultSplitStaticArg"  ((\ n is abs rep assC -> promoteDefR $ wwResultSplitStaticArg n is (mkWWAssC assC) abs rep)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
                [ "Worker/Wrapper Split - Static Argument Variant (Result Variant)",
                  "Perform the static argument transformation on the first n arguments, then perform the worker/wrapper split,",
                  "applying the given abs and rep functions to the specified (by index) static arguments before use."
                ] .+ Introduce .+ Context
    , external "wwResultSplitStaticArgUnsafe" ((\ n is abs rep -> promoteDefR $ wwResultSplitStaticArg n is Nothing abs rep)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore)
                [ "Unsafe Worker/Wrapper Split - Static Argument Variant (Result Variant)",
                  "Perform the static argument transformation on the first n arguments, then perform the (unsafe) worker/wrapper split,",
                  "applying the given abs and rep functions to the specified (by index) static arguments before use."
                ] .+ Introduce .+ Context .+ PreCondition .+ Unsafe
    , external "wwResultAssAToAssB" (promoteExprR . wwResultAssAimpliesAssB . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption B."
                   ]
    , external "wwResultAssBToAssC" (promoteExprR . wwResultAssBimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption B into a proof of worker/wrapper Assumption C."
                   ]
    , external "wwResultAssAToAssC" (promoteExprR . wwResultAssAimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption C."
                   ]
    , external "wwSplit" ((\ wrap unwrap assC -> promoteDefR $ wwSplit (mkWWAssC assC) wrap unwrap)
                                  :: CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
                [ "Worker/Wrapper Split",
                  "For any \"prog :: A\", and given \"wrap :: B -> A\" and \"unwrap :: A -> B\" as arguments,",
                  "and a proof of Assumption C (fix A (\\ a -> wrap (unwrap (f a))) ==> fix A f), then",
                  "prog = expr  ==>  prog = let f = \\ prog -> expr",
                  "                          in let work = unwrap (f (wrap work))",
                  "                              in wrap work"
                ] .+ Introduce .+ Context
    , external "wwSplitUnsafe" ((\ wrap unwrap -> promoteDefR $ wwSplit Nothing wrap unwrap)
                                       :: CoreString -> CoreString -> RewriteH LCore)
                [ "Unsafe Worker/Wrapper Split",
                  "For any \"prog :: A\", and given \"wrap :: B -> A\" and \"unwrap :: A -> B\" as arguments, then",
                  "prog = expr  ==>  prog = let f = \\ prog -> expr",
                  "                          in let work = unwrap (f (wrap work))",
                  "                              in wrap work",
                  "Note: the preCondition \"fix A (wrap . unwrap . f) == fix A f\" is expected to hold."
                ] .+ Introduce .+ Context .+ PreCondition
    , external "wwSplitStaticArg" ((\ n is wrap unwrap assC -> promoteDefR $ wwSplitStaticArg n is (mkWWAssC assC) wrap unwrap)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
                [ "Worker/Wrapper Split - Static Argument Variant",
                  "Perform the static argument transformation on the first n arguments, then perform the worker/wrapper split,",
                  "applying the given wrap and unwrap functions to the specified (by index) static arguments before use."
                ] .+ Introduce .+ Context
    , external "wwSplitStaticArgUnsafe"  ((\ n is wrap unwrap -> promoteDefR $ wwSplitStaticArg n is Nothing wrap unwrap)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore)
                [ "Unsafe Worker/Wrapper Split - Static Argument Variant",
                  "Perform the static argument transformation on the first n arguments, then perform the (unsafe) worker/wrapper split,",
                  "applying the given wrap and unwrap functions to the specified (by index) static arguments before use."
                ] .+ Introduce .+ Context .+ PreCondition

    , external "wwAssAToAssB" (promoteExprR . wwAssAimpliesAssB . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption B."
                   ]
    , external "wwAssBToAssC" (promoteExprR . wwAssBimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption B into a proof of worker/wrapper Assumption C."
                   ]
    , external "wwAssAToAssC" (promoteExprR . wwAssAimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption C."
                   ]
   , external "split1Beta" (\ nm absC -> promoteExprR . parse2BeforeT (split1BetaR Obligation nm) absC :: CoreString -> RewriteH LCore)
        [ "split1Beta <name> <abs expression> <rep expression>"
        , "Perform worker/wrapper split with condition 1-beta."
        , "Given lemma name argument is used as prefix to two introduced lemmas."
        , "  <name>-assumption: unproven lemma for w/w assumption C."
        , "  <name>-fusion: assumed lemma for w/w fusion."
        ]
   , external "split2Beta" (\ nm absC -> promoteExprR . parse2BeforeT (split2BetaR Obligation nm) absC :: CoreString -> RewriteH LCore)
        [ "split2Beta <name> <abs expression> <rep expression>"
        , "Perform worker/wrapper split with condition 2-beta."
        , "Given lemma name argument is used as prefix to two introduced lemmas."
        , "  <name>-assumption: unproven lemma for w/w assumption C."
        , "  <name>-fusion: assumed lemma for w/w fusion."
        ]
    -- HERMIT.API.Dictionary.Unfold
    , external "betaReducePlus" (promoteExprR betaReducePlusR :: RewriteH LCore)
        [ "Perform one or more beta-reductions."]                               .+ Eval .+ Shallow
    , external "unfold" (promoteExprR unfoldR :: RewriteH LCore)
        [ "In application f x y z, unfold f." ] .+ Deep .+ Context
    , external "unfoldWith" (promoteExprR . unfoldNameR . unOccurrenceName :: OccurrenceName -> RewriteH LCore)
        [ "Inline a definition, and apply the arguments; traditional unfold." ] .+ Deep .+ Context
    , external "unfoldAny" (promoteExprR . unfoldNamesR . map unOccurrenceName:: [OccurrenceName] -> RewriteH LCore)
        [ "Unfold a definition if it is named in the list." ] .+ Deep .+ Context
    , external "unfoldSaturated" (promoteExprR unfoldSaturatedR :: RewriteH LCore)
        [ "Unfold a definition only if the function is fully applied." ] .+ Deep .+ Context
    , external "specialize" (promoteExprR specializeR :: RewriteH LCore)
        [ "Specialize an application to its type and coercion arguments." ] .+ Deep .+ Context

    , external "replaceCurrentExprWithUndefined" (promoteExprR replaceCurrentExprWithUndefinedR :: RewriteH LCore)
        [ "Set the current expression to \"undefined\"."
        ] .+ Shallow .+ Context .+ Unsafe
    , external "replaceIdWithUndefined" (promoteCoreR . replaceIdWithUndefined :: HermitName -> RewriteH LCore)
        [ "Replace the specified identifier with \"undefined\"."
        ] .+ Deep .+ Context .+ Unsafe
    , external "errorToUndefined" (promoteExprR errorToUndefinedR :: RewriteH LCore)
        [ "error ty string  ==>  undefined ty"
        ] .+ Shallow .+ Context
    , external "undefinedExpr" (promoteExprR undefinedExprR :: RewriteH LCore)
        [ "undefined-app <+ undefined-lam <+ undefined-let <+ undefined-cast <+ undefined-tick <+ undefined-case"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedApp"  (promoteExprR undefinedAppR :: RewriteH LCore)
        [ "(undefined ty1) e  ==>  undefined ty2"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedLam" (promoteExprR undefinedLamR :: RewriteH LCore)
        [ "(\\ v -> undefined ty1)  ==>  undefined ty2   (where v is not a 'TyVar')"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedLet" (promoteExprR undefinedLetR :: RewriteH LCore)
        [ "let bds in (undefined ty)  ==>  undefined ty"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedCase" (promoteExprR undefinedCaseR :: RewriteH LCore)
        [ "case (undefined ty) of alts  ==>  undefined ty"
        , "OR"
        , "case e of {pat_1 -> undefined ty ; pat_2 -> undefined ty ; ... ; pat_n -> undefined ty} ==> undefined ty"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedCast" (promoteExprR undefinedCastR :: RewriteH LCore)
        [ "Cast (undefined ty1) co  ==>  undefined ty2"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedTick" (promoteExprR undefinedTickR :: RewriteH LCore)
        [ "Tick tick (undefined ty1)  ==>  undefined ty1"
        ] .+ Eval .+ Shallow .+ Context

    , external "foldRule" (promoteExprR . foldRuleR Obligation :: RuleName -> RewriteH LCore)
        [ "Apply a named GHC rule right-to-left." ] .+ Shallow
    , external "foldRules" (promoteExprR . foldRulesR Obligation :: [RuleName] -> RewriteH LCore)
        [ "Apply named GHC rules right-to-left, succeed if any of the rules succeed." ] .+ Shallow
    , external "unfoldRule" (promoteExprR . unfoldRuleR Obligation :: RuleName -> RewriteH LCore)
        [ "Apply a named GHC rule left-to-right." ] .+ Shallow
    , external "unfoldRuleUnsafe" (promoteExprR . unfoldRuleR UnsafeUsed :: RuleName -> RewriteH LCore)
        [ "Apply a named GHC rule left-to-right." ] .+ Shallow .+ Unsafe
    , external "unfoldRules" (promoteExprR . unfoldRulesR Obligation :: [RuleName] -> RewriteH LCore)
        [ "Apply named GHC rules left-to-right, succeed if any of the rules succeed" ] .+ Shallow
    , external "unfoldRulesUnsafe" (promoteExprR . unfoldRulesR UnsafeUsed :: [RuleName] -> RewriteH LCore)
        [ "Apply named GHC rules left-to-right, succeed if any of the rules succeed" ] .+ Shallow .+ Unsafe
    , external "specConstr" (promoteModGutsR specConstrR :: RewriteH LCore)
        [ "Run GHC's SpecConstr pass, which performs call pattern specialization."] .+ Deep
    , external "specialise" (promoteModGutsR specialiseR :: RewriteH LCore)
        [ "Run GHC's specialisation pass, which performs type and dictionary specialisation."] .+ Deep
    , external "mergeQuantifiers" (\n1 n2 -> promoteR (mergeQuantifiersR (cmpHN2Var n1) (cmpHN2Var n2)) :: RewriteH LCore)
        [ "Merge quantifiers from two clauses if they have the same type."
        , "Example:"
        , "(forall (x::Int). foo x = x) ^ (forall (y::Int). bar y y = 5)"
        , "merge-quantifiers 'x 'y"
        , "forall (x::Int). (foo x = x) ^ (bar x x = 5)"
        , "Note: if only one quantifier matches, it will be floated if possible." ]
    , external "floatLeft" (\n1 -> promoteR (mergeQuantifiersR (cmpHN2Var n1) (const False)) :: RewriteH LCore)
        [ "Float quantifier out of left-hand side." ]
    , external "foldRight" (\n1 -> promoteR (mergeQuantifiersR (const False) (cmpHN2Var n1)) :: RewriteH LCore)
        [ "Float quantifier out of right-hand side." ]
    , external "lemmaForward" (forwardT . promoteExprBiR . lemmaBiR Obligation :: LemmaName -> RewriteH LCore)
        [ "Generate a rewrite from a lemma, left-to-right." ]
    , external "lemmaBackward" (backwardT . promoteExprBiR . lemmaBiR Obligation :: LemmaName -> RewriteH LCore)
        [ "Generate a rewrite from a lemma, right-to-left." ]
    , external "lemma-consequent" (promoteClauseR . lemmaConsequentR Obligation :: LemmaName -> RewriteH LCore)
        [ "Match the current lemma with the consequent of an implication lemma."
        , "Upon success, replaces with antecedent of the implication, properly instantiated." ]
    , external "lemmaLhsIntro" (promoteCoreR . lemmaLhsIntroR :: LemmaName -> RewriteH LCore)
        [ "Introduce the LHS of a lemma as a non-recursive binding, in either an expression or a program."
        , "body ==> let v = lhs in body" ] .+ Introduce .+ Shallow
    , external "lemmaRhsIntro" (promoteCoreR . lemmaRhsIntroR :: LemmaName -> RewriteH LCore)
        [ "Introduce the RHS of a lemma as a non-recursive binding, in either an expression or a program."
        , "body ==> let v = rhs in body" ] .+ Introduce .+ Shallow
    , external "instDictionaries" (promoteClauseR instantiateDictsR :: RewriteH LCore)
        [ "Instantiate all of the universally quantified dictionaries of the given lemma." ]
    , external "abstractForall" ((\nm -> promoteClauseR . abstractClauseR nm . csInQBodyT) :: String -> CoreString -> RewriteH LCore)
        [ "Weaken a lemma by abstracting an expression to a new quantifier." ]
    , external "abstractForall" ((\nm rr -> promoteClauseR $ abstractClauseR nm $ extractT rr >>> setFailMsg "path must focus on an expression" projectT) :: String -> RewriteH LCore -> RewriteH LCore)
        [ "Weaken a lemma by abstracting an expression to a new quantifier." ]
    , external "reflexivity" (promoteClauseR (forallR idR reflexivityR <+ reflexivityR) :: RewriteH LCore)
        [ "Rewrite alpha-equivalence to true." ]
    , external "simplifyLemma" (simplifyClauseR :: RewriteH LCore)
        [ "Reduce a proof by applying reflexivity and logical operator identities." ]
    , external "splitAntecedent" (promoteClauseR splitAntecedentR :: RewriteH LCore)
        [ "Split an implication of the form (q1 ^ q2) => q3 into q1 => (q2 => q3)" ]
    , external "lemma" (promoteClauseR . lemmaR Obligation :: LemmaName -> RewriteH LCore)
        [ "Rewrite clause to true using given lemma." ]
    , external "lemmaUnsafe" (promoteClauseR . lemmaR UnsafeUsed :: LemmaName -> RewriteH LCore)
        [ "Rewrite clause to true using given lemma." ] .+ Unsafe
--    , external "crumb" ((\crumb -> (_ignoreResult :: TransformH a b -> RewriteH a) $ transform (\ _hermitC _lcore -> return (singletonSnocPath crumb))) :: Crumb -> RewriteH LCore) -- TODO: Remove hole
--        -- XXX: Is there a good way to avoid exposing this too much?
--        [ "Internal crumb handling system" ]
    , external "lhs" (promoteClauseR . lhsR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the LHS of a quantified clause." ]
    , external "rhs" (promoteClauseR . rhsR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the RHS of a quantified clause." ]
    , external "both" (promoteClauseR . bothR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to both sides of an equality, succeeding if either succeed." ]

    , external "extensionalityWith" (promoteR . extensionalityR . Just :: String -> RewriteH LCore)
        [ "Given a name 'x, then"
        , "f == g  ==>  forall x.  f x == g x" ]

    , external "extensionality" (promoteR (extensionalityR Nothing) :: RewriteH LCore)
        [ "f == g  ==>  forall x.  f x == g x" ]

    , external "pathR" (pathR :: [Crumb] -> RewriteH LCore -> RewriteH LCore)
        [ "Scope a rewrite with a list of Crumbs" ]

    , external "unsafeReplace" (promoteExprR . unsafeReplaceR :: CoreString -> RewriteH LCore)
        [ "replace the currently focused expression with a new expression"
        , "DOES NOT ensure that free variables in the replacement expression are in scope" ]

    , external "serialise"
        (serialise :: [RewriteH LCore] -> RewriteH LCore)
        [ "Sequence Rewrites from left to right" ]
    ]
    where
      mkWWAssC :: RewriteH LCore -> Maybe WWAssumption
      mkWWAssC r = Just (WWAssumption C (extractR r))

-------------------------------------------------------------------------------

instance External (RewriteH LCoreTC) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.Debug
      external "trace"
        (traceR :: String -> RewriteH LCoreTC)
        [ "give a sideEffect message as output when processing this command" ]

    , external "observe"
        (observeR :: String -> RewriteH LCoreTC)
        [ "give a sideEffect message as output, and observe the value " ++
          "being processed" ]

    , external "observeFailure"
        (observeFailureR :: String -> RewriteH LCoreTC -> RewriteH LCoreTC)
        [ "give a sideEffect message if the rewrite fails, including the " ++
          "failing input" ]

    , external "bracket"
        (bracketR :: String -> RewriteH LCoreTC -> RewriteH LCoreTC)
        [ "if given rewrite succeeds, see its input and output" ]

-- HERMIT.API.Dictionary.KURE
    , external "idCoreTC"
        (idR :: RewriteH LCoreTC)
        [ "Perform an identity rewrite."] .+ Shallow

    , external ">>>"
        ((>>>) :: RewriteH LCoreTC -> RewriteH LCoreTC -> RewriteH LCoreTC)
        [ "Compose rewrites, requiring both to succeed." ]

    , external "focus"
        (hfocusR :: TransformH LCoreTC LocalPathH -> RewriteH LCoreTC
                 -> RewriteH LCoreTC)
        [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep

    , external "promote"
        (promoteR :: RewriteH LCore -> RewriteH LCoreTC)
        [ "Promote a RewriteCore to a RewriteCoreTC" ]

    , external "between"
        (betweenR :: Int -> Int -> RewriteH LCoreTC -> RewriteH LCoreTC)
        [ "between x y rr -> perform rr at least x times and at most y times." ]

   , external "atPath"
       (flip hfocusT idR :: TransformH LCoreTC LocalPathH
                         -> TransformH LCoreTC LCoreTC)
       [ "return the expression found at the given path" ]

    , external "unshadowQuantified" (promoteClauseR unshadowClauseR :: RewriteH LCoreTC)
        [ "Unshadow a quantified clause." ]

    , external "serialise"
        (serialise :: [RewriteH LCoreTC] -> RewriteH LCoreTC)
        [ "Sequence Rewrites from left to right" ]
    ]
