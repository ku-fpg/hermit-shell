{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Transform where

import           Control.Arrow
import           Control.Monad

import           Data.Proxy
import           Data.String

import qualified HERMIT.API.Types as API (Name(..))
import           HERMIT.Context
import           HERMIT.Dictionary
import           HERMIT.External (CoreString)
import           HERMIT.GHC
import           HERMIT.Kure
import           HERMIT.Lemma
import           HERMIT.Name
import           HERMIT.ParserCore
import           HERMIT.PrettyPrinter.Common
import           HERMIT.PrettyPrinter.Glyphs
import           HERMIT.Core (Crumb)

import           HERMIT.Server.Parser.Name ()
import           HERMIT.Server.Parser.String ()
import           HERMIT.Server.Parser.Crumb ()
import           HERMIT.Server.Parser.Utils

import           Prelude hiding (abs)
import           Data.Typeable

import           Text.PrettyPrint.MarkedHughesPJ (MDoc) -- TODO: until 7.10
-------------------------------------------------------------------------------

instance External (BiRewriteH LCore) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.FixPoint
      external "fixComputationRule"
        (promoteExprBiR fixComputationRuleBR :: BiRewriteH LCore)

    , external "fixRollingRule"
        (promoteExprBiR fixRollingRuleBR :: BiRewriteH LCore)

    , external "fixFusionRule"
        ((\ f g h r1 r2 strictf ->
            promoteExprBiR (fixFusionRule (Just (r1,r2)) (Just strictf) f g h))
        :: CoreString -> CoreString -> CoreString -> RewriteH LCore
        -> RewriteH LCore -> RewriteH LCore -> BiRewriteH LCore)

-- HERMIT.API.Dictionary.KURE
    , external ">>>"
        ((>>>) :: BiRewriteH LCore -> BiRewriteH LCore -> BiRewriteH LCore)

    , external "invert"
        (invertBiT :: BiRewriteH LCore -> BiRewriteH LCore)

-- ??
    , external "wwResultFactorisation" ((\ abs rep assC -> promoteExprBiR $ wwFac (mkWWAssC assC) abs rep)
                                          :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
    , external "wwResultFactorisationUnsafe" ((\ wrap unwrap -> promoteExprBiR $ wwFac Nothing wrap unwrap)
                                               :: CoreString -> CoreString -> BiRewriteH LCore)
    , external "wwResultAssumptionA" ((\ abs rep assA -> promoteExprBiR $ wwA (Just $ extractR assA) abs rep)
                                       :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
    , external "wwResultAssumptionB" ((\ abs rep f assB -> promoteExprBiR $ wwB (Just $ extractR assB) abs rep f)
                                       :: CoreString -> CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
    , external "wwResultAssumptionC" ((\ abs rep f assC -> promoteExprBiR $ wwC (Just $ extractR assC) abs rep f)
                                       :: CoreString -> CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
    , external "wwResultAssumptionAUnsafe" ((\ abs rep -> promoteExprBiR $ wwA Nothing abs rep)
                                              :: CoreString -> CoreString -> BiRewriteH LCore)
    , external "wwResultAssumptionBUnsafe" ((\ abs rep f -> promoteExprBiR $ wwB Nothing abs rep f)
                                              :: CoreString -> CoreString -> CoreString -> BiRewriteH LCore)
    , external "wwResultFusion" (promoteExprBiR wwFusion :: BiRewriteH LCore)

    , external "wwFactorisation" ((\ wrap unwrap assC -> promoteExprBiR $ wwFac (mkWWAssC assC) wrap unwrap)
                                          :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
    , external "wwFactorisationUnsafe" ((\ wrap unwrap -> promoteExprBiR $ wwFac Nothing wrap unwrap)
                                               :: CoreString -> CoreString -> BiRewriteH LCore)
    , external "wwAssumptionA" ((\ wrap unwrap assA -> promoteExprBiR $ wwA (Just $ extractR assA) wrap unwrap)
                                       :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
    , external "wwAssumptionB" ((\ wrap unwrap f assB -> promoteExprBiR $ wwB (Just $ extractR assB) wrap unwrap f)
                                       :: CoreString -> CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
    , external "wwAssumptionC" ((\ wrap unwrap f assC -> promoteExprBiR $ wwC (Just $ extractR assC) wrap unwrap f)
                                       :: CoreString -> CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
    , external "wwAssumptionAUnsafe" ((\ wrap unwrap -> promoteExprBiR $ wwA Nothing wrap unwrap)
                                              :: CoreString -> CoreString -> BiRewriteH LCore)
    , external "wwAssumptionBUnsafe" ((\ wrap unwrap f -> promoteExprBiR $ wwB Nothing wrap unwrap f)
                                              :: CoreString -> CoreString -> CoreString -> BiRewriteH LCore)
    , external "wwAssumptionCUnsafe" ((\ wrap unwrap f -> promoteExprBiR $ wwC Nothing wrap unwrap f)
                                              :: CoreString -> CoreString -> CoreString -> BiRewriteH LCore)
    , external "wwFusion" (promoteExprBiR wwFusion :: BiRewriteH LCore)

    , external "retraction" ((\ f g r -> promoteExprBiR $ retraction (Just r) f g) :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
    , external "retractionUnsafe" ((\ f g -> promoteExprBiR $ retraction Nothing f g) :: CoreString -> CoreString -> BiRewriteH LCore)
    , external "lemmaBirewrite" (promoteExprBiR . lemmaBiR Obligation :: LemmaName -> BiRewriteH LCore)
    , external "lemmaConsequentBirewrite" (promoteExprBiR . lemmaConsequentBiR Obligation :: LemmaName -> BiRewriteH LCore)
    ]
    where
      mkWWAssC :: RewriteH LCore -> Maybe WWAssumption
      mkWWAssC r = Just (WWAssumption C (extractR r))

-- | For any @f :: A -> A@, and given @wrap :: B -> A@ and @unwrap :: A -> B@ as arguments, then
--   @fix A f@  \<==\>  @wrap (fix B (\\ b -> unwrap (f (wrap b))))@
wwFac :: Maybe WWAssumption -> CoreString -> CoreString -> BiRewriteH CoreExpr
wwFac mAss = parse2beforeBiR (wwFacBR mAss)

instance External (RewriteH LCore) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.AlphaConversion
      external "alpha"
        (promoteCoreR alphaR :: RewriteH LCore)

    , external "alphaLam"
        (promoteExprR . alphaLamR :: Maybe String -> RewriteH LCore)

    , external "alphaCaseBinder"
        (promoteExprR . alphaCaseBinderR :: Maybe String -> RewriteH LCore)

    , external "alphaAlt"
        (promoteAltR alphaAltR :: RewriteH LCore)

    , external "alphaAltWith"
        (promoteAltR . alphaAltWithR :: [String] -> RewriteH LCore)

    , external "alphaCase"
        (promoteExprR alphaCaseR :: RewriteH LCore)

    , external "alphaLetWith"
        (promoteExprR . alphaLetWithR :: [String] -> RewriteH LCore)

    , external "alphaLet"
        (promoteExprR alphaLetR :: RewriteH LCore)

    , external "alphaTopWith"
        (promoteProgR . alphaProgConsWithR :: [String] -> RewriteH LCore)

    , external "alphaTop"
        (promoteProgR alphaProgConsR :: RewriteH LCore)

    , external "alphaProg"
        (promoteProgR alphaProgR :: RewriteH LCore)

    , external "unshadow"
        (promoteCoreR unshadowR :: RewriteH LCore)

-- HERMIT.API.Dictionary.Composite
    , external "unfoldBasicCombinator"
        (promoteExprR unfoldBasicCombinatorR :: RewriteH LCore)

    , external "simplify"
        (simplifyR :: RewriteH LCore)

    , external "bash"
        (bashR :: RewriteH LCore)

    , external "smash"
        (smashR :: RewriteH LCore)

    , external "bashExtendedWith"
        (bashExtendedWithR :: [RewriteH LCore] -> RewriteH LCore)

    , external "smashExtendedWith"
        (smashExtendedWithR :: [RewriteH LCore] -> RewriteH LCore)

    , external "bashDebug"
        (bashDebugR :: RewriteH LCore)

-- HERMIT.API.Dictionary.FixPoint
    , external "fixIntro"
        (promoteCoreR fixIntroR :: RewriteH LCore)

-- HERMIT.API.Dictionary.Fold
    , external "fold"
        (promoteExprR . foldR :: HermitName -> RewriteH LCore)

-- HERMIT.API.Dictionary.Function
    , external "staticArg"
        (promoteDefR staticArgR :: RewriteH LCore)

    , external "staticArgTypes"
        (promoteDefR staticArgTypesR :: RewriteH LCore)

    , external "staticArgPos"
        (promoteDefR . staticArgPosR :: [Int] -> RewriteH LCore)

-- HERMIT.API.Dictionary.GHC
    , external "deshadowProg"
        (promoteProgR deShadowProgR :: RewriteH LCore)

    , external "dezombify"
        (promoteExprR dezombifyR :: RewriteH LCore)

    , external "occurrenceAnalysis"
        (occurrenceAnalysisR :: RewriteH LCore)

-- HERMIT.API.Dictionary.Induction
    , external "caseSplitOn"
        ((\ fl -> promoteClauseR . caseSplitOnR fl . cmpHN2Var)
         :: Bool -> HermitName -> RewriteH LCore)

-- HERMIT.API.Dictionary.Inline
    , external "inline"
        (promoteExprR inlineR :: RewriteH LCore)

    , external "inlineWith"
        (promoteExprR . inlineMatchingPredR . cmpHN2Var :: HermitName
                                                        -> RewriteH LCore)

    , external "inlineAny"
        (promoteExprR . inlineNamesR :: [String] -> RewriteH LCore)

    , external "inlineCaseScrutinee"
        (promoteExprR inlineCaseScrutineeR :: RewriteH LCore)

    , external "inlineCaseAlternative"
        (promoteExprR inlineCaseAlternativeR :: RewriteH LCore)

-- HERMIT.API.Dictionary.KURE
    , external "idCore"
        (idR :: RewriteH LCore)

    , external "fail_"
        (fail :: String -> RewriteH LCore)

    , external "<+"
        ((<+) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)

    , external ">>>"
        ((>>>) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)

    , external ">+>"
        ((>+>) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)

    , external "try"
        (tryR :: RewriteH LCore -> RewriteH LCore)

    , external "repeat"
        (repeatR :: RewriteH LCore -> RewriteH LCore)

    , external "replicate"
        ((\ n -> andR . replicate n) :: Int -> RewriteH LCore -> RewriteH LCore)

    , external "all"
        (allR :: RewriteH LCore -> RewriteH LCore)

    , external "any"
        (anyR :: RewriteH LCore -> RewriteH LCore)

    , external "one"
        (oneR :: RewriteH LCore -> RewriteH LCore)

    , external "allBU"
        (allbuR :: RewriteH LCore -> RewriteH LCore)

    , external "allTD"
        (alltdR :: RewriteH LCore -> RewriteH LCore)

    , external "allDU"
        (allduR :: RewriteH LCore -> RewriteH LCore)

    , external "anyBU"
        (anybuR :: RewriteH LCore -> RewriteH LCore)

    , external "anyTD"
        (anytdR :: RewriteH LCore -> RewriteH LCore)

    , external "anyDU"
        (anyduR :: RewriteH LCore -> RewriteH LCore)

    , external "oneTD"
        (onetdR :: RewriteH LCore -> RewriteH LCore)

    , external "oneBU"
        (onebuR :: RewriteH LCore -> RewriteH LCore)

    , external "pruneTD"
        (prunetdR :: RewriteH LCore -> RewriteH LCore)

    , external "innermost"
        (innermostR :: RewriteH LCore -> RewriteH LCore)

    , external "focus"
        (hfocusR :: TransformH LCore LocalPathH -> RewriteH LCore
                 -> RewriteH LCore)

    , external "when"
        ((>>) :: TransformH LCore () -> RewriteH LCore -> RewriteH LCore)

    , external "forward"
        (forwardT :: BiRewriteH LCore -> RewriteH LCore)

    , external "backward"
        (backwardT :: BiRewriteH LCore -> RewriteH LCore)

    , external "anyCall"
        (const anyCallR_LCore :: Proxy LCore -> RewriteH LCore
                              -> RewriteH LCore)

    , external "extractR"
        (extractR :: RewriteH LCoreTC -> RewriteH LCore)

    , external "atPath"
        (flip hfocusT idR :: TransformH LCore LocalPathH
                          -> TransformH LCore LCore)

    , external "atPathProj"
        (extractT . flip hfocusT projectT :: TransformH LCoreTC LocalPathH
                                          -> TransformH LCore LCore)

      -- HERMIT.API.Dictionary.Local
    , external "betaReduce" (promoteExprR betaReduceR :: RewriteH LCore)
    , external "betaExpand" (promoteExprR betaExpandR :: RewriteH LCore)
    , external "etaReduce" (promoteExprR etaReduceR :: RewriteH LCore)
    , external "etaExpand" (promoteExprR . etaExpandR :: String -> RewriteH LCore)
    , external "flattenModule" (promoteModGutsR flattenModuleR :: RewriteH LCore)
    , external "flattenProgram" (promoteProgR flattenProgramR :: RewriteH LCore)
    , external "abstract" (promoteExprR . abstractR . mkOccPred :: OccurrenceName -> RewriteH LCore)
    , external "push" ((\ nm strictf -> push (Just strictf) (cmpString2Var nm)) :: String -> RewriteH LCore -> RewriteH LCore)
    , external "pushUnsafe" (push Nothing . cmpString2Var :: String -> RewriteH LCore)

-- HERMIT.API.Dictionary.Local.Bind
    , external "nonrecToRec"
        (promoteBindR nonrecToRecR :: RewriteH LCore)

    , external "recToNonrec"
        (promoteBindR recToNonrecR :: RewriteH LCore)

-- HERMIT.API.Dictionary.Local.Case
    , external "caseFloatApp"
        (promoteExprR caseFloatAppR :: RewriteH LCore)

    , external "caseFloatArg"
        ((\ x -> promoteExprR . caseFloatArg x)
         :: Maybe CoreString -> Maybe (RewriteH LCore) -> RewriteH LCore)

    , external "caseFloatArgLemma"
        ((\ x -> promoteExprR . caseFloatArgLemmaR x)
          :: Used -> LemmaName -> RewriteH LCore)

    , external "caseFloatCase"
        (promoteExprR caseFloatCaseR :: RewriteH LCore)

    , external "caseFloatCast"
        (promoteExprR caseFloatCastR :: RewriteH LCore)

    , external "caseFloatLet"
        (promoteExprR caseFloatLetR :: RewriteH LCore)

    , external "caseFloat"
        (promoteExprR caseFloatR :: RewriteH LCore)

    , external "caseFloatIn"
        (promoteExprR caseFloatInR :: RewriteH LCore)

    , external "caseFloatInArgs"
        (promoteExprR caseFloatInArgsR :: RewriteH LCore)

    , external "caseReduce"
        (promoteExprR (caseReduceR True) :: RewriteH LCore)

    , external "caseReduceDatacon"
        (promoteExprR (caseReduceDataconR True) :: RewriteH LCore)

    , external "caseReduceLiteral"
        (promoteExprR (caseReduceLiteralR True) :: RewriteH LCore)

    , external "caseReduceUnfold"
        (promoteExprR (caseReduceUnfoldR True) :: RewriteH LCore)

    , external "caseSplit"
        ((\nm -> findVarT (unOccurrenceName nm) >>= promoteExprR . caseSplitR .
                 varToCoreExpr) :: OccurrenceName -> RewriteH LCore)

    , external "caseSplitQQ"
        (parseCoreExprT >=> promoteR .
         caseSplitR :: CoreString -> RewriteH LCore)

    , external "caseSplitInline"
        ((\nm -> findVarT (unOccurrenceName nm) >>= promoteExprR .
                 caseSplitInlineR .
                 varToCoreExpr) :: OccurrenceName -> RewriteH LCore)

    , external "caseSplitInlineQQ"
        (parseCoreExprT >=> promoteExprR .
         caseSplitInlineR :: CoreString -> RewriteH LCore)

    , external "caseIntroSeq"
        (promoteExprR . caseIntroSeqR .
         cmpString2Var :: String -> RewriteH LCore)

    , external "caseElimSeq"
        (promoteExprR caseElimSeqR :: RewriteH LCore)

    , external "caseInlineAlternative"
        (promoteExprR caseInlineAlternativeR :: RewriteH LCore)

    , external "caseInlineScrutinee"
        (promoteExprR caseInlineScrutineeR :: RewriteH LCore)

    , external "caseMergeAlts"
        (promoteExprR caseMergeAltsR :: RewriteH LCore)

    , external "caseMergeAltsWithBinder"
        (promoteExprR caseMergeAltsWithBinderR :: RewriteH LCore)

    , external "caseElim"
        (promoteExprR caseElimR :: RewriteH LCore)

    , external "caseElimInlineScrutinee"
        (promoteExprR caseElimInlineScrutineeR :: RewriteH LCore)

    , external "caseElimMergeAlts"
        (promoteExprR caseElimMergeAltsR :: RewriteH LCore)

    , external "caseFoldBinder"
        (promoteExprR caseFoldBinderR :: RewriteH LCore)

-- HERMIT.API.Dictionary.Local.Cast
    , external "castElim"
        (promoteExprR castElimR :: RewriteH LCore)

    , external "castElimRefl"
        (promoteExprR castElimReflR :: RewriteH LCore)

    , external "castElimSym"
        (promoteExprR castElimSymR :: RewriteH LCore)

    , external "castElimSymPlus"
        (promoteExprR castElimSymPlusR :: RewriteH LCore)

    , external "castFloatApp"
        (promoteExprR castFloatAppR :: RewriteH LCore)

    , external "castFloatLam"
        (promoteExprR castFloatLamR :: RewriteH LCore)

    , external "castElimUnsafe"
        (promoteExprR castElimUnsafeR :: RewriteH LCore)

-- HERMIT.API.Dictionary.Local.Let
    , external "letSubst"
        (promoteExprR letSubstR :: RewriteH LCore)

    , external "letSubstSafe"
        (promoteExprR letSubstSafeR :: RewriteH LCore)

    , external "letNonrecSubstSafe"
        (promoteExprR letNonRecSubstSafeR :: RewriteH LCore)

    , external "letIntro"
        (promoteExprR . letIntroR :: String -> RewriteH LCore)

    , external "letIntroUnfolding"
        (promoteExprR . letIntroUnfoldingR :: HermitName -> RewriteH LCore)

    , external "letElim"
        (promoteExprR letElimR :: RewriteH LCore)

    , external "letFloatApp"
        (promoteExprR letFloatAppR :: RewriteH LCore)

    , external "letFloatArg"
        (promoteExprR letFloatArgR :: RewriteH LCore)

    , external "letFloatLam"
        (promoteExprR letFloatLamR :: RewriteH LCore)

    , external "letFloatLet"
        (promoteExprR letFloatLetR :: RewriteH LCore)

    , external "letFloatCase"
        (promoteExprR letFloatCaseR :: RewriteH LCore)

    , external "letFloatCaseAlt"
        (promoteExprR . letFloatCaseAltR :: Maybe Int -> RewriteH LCore)

    , external "letFloatCast"
        (promoteExprR letFloatCastR :: RewriteH LCore)

    , external "letFloatTop"
        (promoteProgR letFloatTopR :: RewriteH LCore)

    , external "letFloat"
        (promoteProgR letFloatTopR <+
         promoteExprR letFloatExprR :: RewriteH LCore)

    , external "letToCase"
        (promoteExprR letToCaseR :: RewriteH LCore)

    , external "letFloatIn"
        (promoteExprR letFloatInR >+>
         anybuR (promoteExprR letElimR) :: RewriteH LCore)

    , external "letFloatInApp"
        ((promoteExprR letFloatInAppR >+>
          anybuR (promoteExprR letElimR)) :: RewriteH LCore)

    , external "letFloatInCase"
        ((promoteExprR letFloatInCaseR >+>
          anybuR (promoteExprR letElimR)) :: RewriteH LCore)

    , external "letFloatInLam"
        ((promoteExprR letFloatInLamR >+>
          anybuR (promoteExprR letElimR)) :: RewriteH LCore)

    , external "reorderLets"
         (promoteExprR . reorderNonRecLetsR :: [String] -> RewriteH LCore)

    , external "letTuple"
        (promoteExprR . letTupleR :: String -> RewriteH LCore)

    , external "progBindElim"
        (promoteProgR progBindElimR :: RewriteH LCore)

    , external "progBindNonrecElim"
        (promoteProgR progBindNonRecElimR :: RewriteH LCore)

    , external "progBindRecElim"
        (promoteProgR progBindRecElimR :: RewriteH LCore)

-- HERMIT.API.Dictionary.New
    , external "nonrecIntro"
        ((\ s str -> promoteCoreR (nonRecIntro s str))
         :: String -> CoreString -> RewriteH LCore)

      -- HERMIT.API.Dictionary.Remembered
    , external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)

    , external "foldRemembered" (promoteExprR . foldRememberedR Obligation :: LemmaName -> RewriteH LCore)

    , external "foldAnyRemembered" (promoteExprR foldAnyRememberedR :: RewriteH LCore)

    , external "wwResultSplit" ((\ abs rep assC -> promoteDefR $ wwResultSplit (mkWWAssC assC) abs rep)
                                  :: CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
    , external "wwResultSplitUnsafe" ((\ abs rep -> promoteDefR $ wwResultSplit Nothing abs rep)
                                       :: CoreString -> CoreString -> RewriteH LCore)
    , external "wwResultSplitStaticArg"  ((\ n is abs rep assC -> promoteDefR $ wwResultSplitStaticArg n is (mkWWAssC assC) abs rep)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
    , external "wwResultSplitStaticArgUnsafe" ((\ n is abs rep -> promoteDefR $ wwResultSplitStaticArg n is Nothing abs rep)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore)
    , external "wwResultAssAToAssB" (promoteExprR . wwResultAssAimpliesAssB . extractR :: RewriteH LCore -> RewriteH LCore)
    , external "wwResultAssBToAssC" (promoteExprR . wwResultAssBimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
    , external "wwResultAssAToAssC" (promoteExprR . wwResultAssAimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
    , external "wwSplit" ((\ wrap unwrap assC -> promoteDefR $ wwSplit (mkWWAssC assC) wrap unwrap)
                                  :: CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
    , external "wwSplitUnsafe" ((\ wrap unwrap -> promoteDefR $ wwSplit Nothing wrap unwrap)
                                       :: CoreString -> CoreString -> RewriteH LCore)
    , external "wwSplitStaticArg" ((\ n is wrap unwrap assC -> promoteDefR $ wwSplitStaticArg n is (mkWWAssC assC) wrap unwrap)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
    , external "wwSplitStaticArgUnsafe"  ((\ n is wrap unwrap -> promoteDefR $ wwSplitStaticArg n is Nothing wrap unwrap)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore)

    , external "wwAssAToAssB" (promoteExprR . wwAssAimpliesAssB . extractR :: RewriteH LCore -> RewriteH LCore)
    , external "wwAssBToAssC" (promoteExprR . wwAssBimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
    , external "wwAssAToAssC" (promoteExprR . wwAssAimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
    , external "split1Beta" (\ nm absC -> promoteExprR . parse2BeforeT (split1BetaR Obligation nm) absC :: CoreString -> RewriteH LCore)
    , external "split2Beta" (\ nm absC -> promoteExprR . parse2BeforeT (split2BetaR Obligation nm) absC :: CoreString -> RewriteH LCore)
    -- HERMIT.API.Dictionary.Unfold
    , external "betaReducePlus" (promoteExprR betaReducePlusR :: RewriteH LCore)
    , external "unfold" (promoteExprR unfoldR :: RewriteH LCore)
    , external "unfoldWith" (promoteExprR . unfoldNameR . unOccurrenceName :: OccurrenceName -> RewriteH LCore)
    , external "unfoldAny" (promoteExprR . unfoldNamesR . map unOccurrenceName:: [OccurrenceName] -> RewriteH LCore)
    , external "unfoldSaturated" (promoteExprR unfoldSaturatedR :: RewriteH LCore)
    , external "specialize" (promoteExprR specializeR :: RewriteH LCore)

    , external "replaceCurrentExprWithUndefined" (promoteExprR replaceCurrentExprWithUndefinedR :: RewriteH LCore)
    , external "replaceIdWithUndefined" (promoteCoreR . replaceIdWithUndefined :: HermitName -> RewriteH LCore)
    , external "errorToUndefined" (promoteExprR errorToUndefinedR :: RewriteH LCore)
    , external "undefinedExpr" (promoteExprR undefinedExprR :: RewriteH LCore)
    , external "undefinedApp"  (promoteExprR undefinedAppR :: RewriteH LCore)
    , external "undefinedLam" (promoteExprR undefinedLamR :: RewriteH LCore)
    , external "undefinedLet" (promoteExprR undefinedLetR :: RewriteH LCore)
    , external "undefinedCase" (promoteExprR undefinedCaseR :: RewriteH LCore)
    , external "undefinedCast" (promoteExprR undefinedCastR :: RewriteH LCore)
    , external "undefinedTick" (promoteExprR undefinedTickR :: RewriteH LCore)

    , external "foldRule" (promoteExprR . foldRuleR Obligation :: RuleName -> RewriteH LCore)
    , external "foldRules" (promoteExprR . foldRulesR Obligation :: [RuleName] -> RewriteH LCore)
    , external "unfoldRule" (promoteExprR . unfoldRuleR Obligation :: RuleName -> RewriteH LCore)
    , external "unfoldRuleUnsafe" (promoteExprR . unfoldRuleR UnsafeUsed :: RuleName -> RewriteH LCore)
    , external "unfoldRules" (promoteExprR . unfoldRulesR Obligation :: [RuleName] -> RewriteH LCore)
    , external "unfoldRulesUnsafe" (promoteExprR . unfoldRulesR UnsafeUsed :: [RuleName] -> RewriteH LCore)
    , external "specConstr" (promoteModGutsR specConstrR :: RewriteH LCore)
    , external "specialise" (promoteModGutsR specialiseR :: RewriteH LCore)
    , external "mergeQuantifiers" (\n1 n2 -> promoteR (mergeQuantifiersR (cmpHN2Var n1) (cmpHN2Var n2)) :: RewriteH LCore)
    , external "floatLeft" (\n1 -> promoteR (mergeQuantifiersR (cmpHN2Var n1) (const False)) :: RewriteH LCore)
    , external "foldRight" (\n1 -> promoteR (mergeQuantifiersR (const False) (cmpHN2Var n1)) :: RewriteH LCore)
    , external "lemmaForward" (forwardT . promoteExprBiR . lemmaBiR Obligation :: LemmaName -> RewriteH LCore)
    , external "lemmaBackward" (backwardT . promoteExprBiR . lemmaBiR Obligation :: LemmaName -> RewriteH LCore)
    , external "lemma-consequent" (promoteClauseR . lemmaConsequentR Obligation :: LemmaName -> RewriteH LCore)
    , external "lemmaLhsIntro" (promoteCoreR . lemmaLhsIntroR :: LemmaName -> RewriteH LCore)
    , external "lemmaRhsIntro" (promoteCoreR . lemmaRhsIntroR :: LemmaName -> RewriteH LCore)
    , external "instDictionaries" (promoteClauseR instantiateDictsR :: RewriteH LCore)
    , external "abstractForall" ((\nm -> promoteClauseR . abstractClauseR nm . csInQBodyT) :: String -> CoreString -> RewriteH LCore)
    , external "abstractForall" ((\nm rr -> promoteClauseR $ abstractClauseR nm $ extractT rr >>> setFailMsg "path must focus on an expression" projectT) :: String -> RewriteH LCore -> RewriteH LCore)
    , external "reflexivity" (promoteClauseR (forallR idR reflexivityR <+ reflexivityR) :: RewriteH LCore)
    , external "simplifyLemma" (simplifyClauseR :: RewriteH LCore)
    , external "splitAntecedent" (promoteClauseR splitAntecedentR :: RewriteH LCore)
    , external "lemma" (promoteClauseR . lemmaR Obligation :: LemmaName -> RewriteH LCore)
    , external "lemmaUnsafe" (promoteClauseR . lemmaR UnsafeUsed :: LemmaName -> RewriteH LCore)
--    , external' "crumb" ((\crumb -> (_ignoreResult :: TransformH a b -> RewriteH a) $ transform (\ _hermitC _lcore -> return (singletonSnocPath crumb))) :: Crumb -> RewriteH LCore) -- TODO: Remove hole
--        -- XXX: Is there a good way to avoid exposing this too much?
--        [ "Internal crumb handling system" ]
    , external "lhs" (promoteClauseR . lhsR :: RewriteH LCore -> RewriteH LCore)
    , external "rhs" (promoteClauseR . rhsR :: RewriteH LCore -> RewriteH LCore)
    , external "both" (promoteClauseR . bothR :: RewriteH LCore -> RewriteH LCore)

    , external "extensionalityWith" (promoteR . extensionalityR . Just :: String -> RewriteH LCore)

    , external "extensionality" (promoteR (extensionalityR Nothing) :: RewriteH LCore)

    , external "pathR" (pathR :: [Crumb] -> RewriteH LCore -> RewriteH LCore)

    , external "unsafeReplace" (promoteExprR . unsafeReplaceR :: CoreString -> RewriteH LCore)

    , external "serialise"
        (serialise :: [RewriteH LCore] -> RewriteH LCore)
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

    , external "observe"
        (observeR :: String -> RewriteH LCoreTC)

    , external "observeFailure"
        (observeFailureR :: String -> RewriteH LCoreTC -> RewriteH LCoreTC)

    , external "bracket"
        (bracketR :: String -> RewriteH LCoreTC -> RewriteH LCoreTC)

-- HERMIT.API.Dictionary.KURE
    , external "idCoreTC"
        (idR :: RewriteH LCoreTC)

    , external ">>>"
        ((>>>) :: RewriteH LCoreTC -> RewriteH LCoreTC -> RewriteH LCoreTC)

    , external "focus"
        (hfocusR :: TransformH LCoreTC LocalPathH -> RewriteH LCoreTC
                 -> RewriteH LCoreTC)

    , external "promote"
        (promoteR :: RewriteH LCore -> RewriteH LCoreTC)

    , external "between"
        (betweenR :: Int -> Int -> RewriteH LCoreTC -> RewriteH LCoreTC)

    , external "atPath"
       (flip hfocusT idR :: TransformH LCoreTC LocalPathH
                         -> TransformH LCoreTC LCoreTC)

    , external "unshadowQuantified" (promoteClauseR unshadowClauseR :: RewriteH LCoreTC)

    , external "serialise"
        (serialise :: [RewriteH LCoreTC] -> RewriteH LCoreTC)
    ]

-------------------------------------------------------------------------------

instance External (TransformH LCore LocalPathH) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.Navigation
      external "consider"
        (considerConstructT :: Considerable -> TransformH LCore LocalPathH)

    , external "arg"
        (promoteExprT . nthArgPath :: Int -> TransformH LCore LocalPathH)

    , external "lamsBody"
        (promoteExprT lamsBodyT :: TransformH LCore LocalPathH)

    , external "letsBody"
        (promoteExprT letsBodyT :: TransformH LCore LocalPathH)

    , external "progEnd"
        (promoteModGutsT gutsProgEndT <+
         promoteProgT progEndT :: TransformH LCore LocalPathH)

    , external "parentOfCore"
        (parentOfT :: TransformH LCore LocalPathH
                   -> TransformH LCore LocalPathH)
    ]

instance External (TransformH LCoreTC LocalPathH) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.Navigation
      external "rhsOf"
        (rhsOfT . mkRhsOfPred :: RhsOfName -> TransformH LCoreTC LocalPathH)

    , external "bindingGroupOf"
        (bindingGroupOfT .
         cmpString2Var :: String -> TransformH LCoreTC LocalPathH)

    , external "bindingOf"
        (bindingOfT .
         mkBindingPred :: BindingName -> TransformH LCoreTC LocalPathH)

    , external "occurrenceOf"
        (occurrenceOfT .
         mkOccPred :: OccurrenceName -> TransformH LCoreTC LocalPathH)

    , external "applicationOf"
        (applicationOfT .
         mkOccPred :: OccurrenceName -> TransformH LCoreTC LocalPathH)

    , external "parentOfCoreTC"
        (parentOfT :: TransformH LCoreTC LocalPathH
                   -> TransformH LCoreTC LocalPathH)
    ]

instance External (TransformH LCoreTC String) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.GHC
      external "lintExpr"
        (promoteExprT lintExprT :: TransformH LCoreTC String)

    , external "lintModule"
        (promoteModGutsT lintModuleT :: TransformH LCoreTC String)

    , external "lint"
        (promoteT lintClauseT :: TransformH LCoreTC String)

-- HERMIT.API.Dictionary.KURE
    , external "focus"
        (hfocusT :: TransformH LCoreTC LocalPathH -> TransformH LCoreTC String
                 -> TransformH LCoreTC String)

    , external "showRules" (rulesHelpListT :: TransformH LCoreTC String)

-- HERMIT.API.Dictionary.Query
    , external "info"
        (promoteCoreTCT infoT :: TransformH LCoreTC String)
    ]

instance External (TransformH LCore ()) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.GHC
      external "injectDependency"
        (promoteModGutsT . injectDependencyT . mkModuleName
         :: String -> TransformH LCore ())

-- HERMIT.API.Dictionary.KURE
    , external "success"
        (successT :: TransformH LCore ())

    , external "<+"
        ((<+) :: TransformH LCore () -> TransformH LCore ()
              -> TransformH LCore ())

    , external "not_"
       (notM :: TransformH LCore () -> TransformH LCore ())

-- HERMIT.API.Dictionary.New
    , external "var"
        (promoteExprT . isVar :: String -> TransformH LCore ())

      -- HERMIT.API.Dictionary.Remembered
    , external "remember" (promoteCoreT . rememberR :: LemmaName -> TransformH LCore ()) -- Done not smell right (return ()?)

    , external "wwResultGenerateFusion" (wwResultGenerateFusionT . mkWWAssC :: RewriteH LCore -> TransformH LCore ())
    , external "wwResultGenerateFusionUnsafe" (wwResultGenerateFusionT Nothing :: TransformH LCore ())
    , external "wwGenerateFusion" (wwGenerateFusionT . mkWWAssC :: RewriteH LCore -> TransformH LCore ())
    , external "wwGenerateFusionUnsafe" (wwGenerateFusionT Nothing :: TransformH LCore ())

    , external "introWWAssumptionA"
      (\nm absC repC -> do
            q <- parse2BeforeT assumptionAClauseT absC repC
            insertLemmaT nm $ Lemma q NotProven NotUsed :: TransformH LCore ())
    , external "introWWAssumptionB"
      (\nm absC repC bodyC -> do
            q <- parse3BeforeT assumptionBClauseT absC repC bodyC
            insertLemmaT nm $ Lemma q NotProven NotUsed :: TransformH LCore ())
    , external "introWWAssumptionC"
      (\nm absC repC bodyC -> do
            q <- parse3BeforeT assumptionCClauseT absC repC bodyC
            insertLemmaT nm $ Lemma q NotProven NotUsed :: TransformH LCore ())
    , external "isUndefinedVal" (promoteExprT isUndefinedValT :: TransformH LCore ())
    , external "imply" (\n1 n2 n3 -> implyLemmasT n1 n2 n3 :: TransformH LCore ())
    , external "instLemma" (\ nm v cs -> modifyLemmaT nm id (instantiateClauseVarR (cmpHN2Var v) cs) id id :: TransformH LCore ())
    , external "copyLemma" (\ nm newName -> modifyLemmaT nm (const newName) idR id id :: TransformH LCore ())
    , external "modifyLemma" ((\ nm rr -> modifyLemmaT nm id (extractR rr) (const NotProven) (const NotUsed)) :: LemmaName -> RewriteH LCore -> TransformH LCore ())
    , external "conjunct" (\n1 n2 n3 -> conjunctLemmasT n1 n2 n3 :: TransformH LCore ())
    , external "disjunct" (\n1 n2 n3 -> disjunctLemmasT n1 n2 n3 :: TransformH LCore ())
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

-- HERMIT.API.Dictionary.KURE
    , external "focus"
        (hfocusT :: TransformH LCore LocalPathH -> TransformH LCore String
                 -> TransformH LCore String)

    , external "test"
        (testQuery :: RewriteH LCore -> TransformH LCore String)

    , external "extractT"
        (extractT :: TransformH LCoreTC String -> TransformH LCore String)

    , external "queryLemma" ((\ nm t -> getLemmaByNameT nm >>> arr lemmaC >>> extractT t) :: LemmaName -> TransformH LCore String -> TransformH LCore String)

    , external "lhs" (promoteClauseT . lhsT :: TransformH LCore String -> TransformH LCore String)
    , external "rhs" (promoteClauseT . rhsT :: TransformH LCore String -> TransformH LCore String)
    , external "both" ((\t -> do (r,s) <- promoteClauseT (bothT t); return (unlines [r,s])) :: TransformH LCore String -> TransformH LCore String)
    ]

instance External (TransformH LCore Glyphs) where
  parseExternals =
    [ external "ruleToLemma"
        (lemmaHelpT :: PrettyPrinter -> RuleName -> TransformH LCore Glyphs)
    ]


instance External (TransformH LCoreTC Glyphs) where
  parseExternals =
    [ external "showRule"
        (ruleHelpT :: PrettyPrinter -> RuleName -> TransformH LCoreTC Glyphs)
    ]

instance External (TransformH LCoreTC ()) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.Query
      external "compareBoundIds"
        (compareBoundIds :: HermitName -> HermitName -> TransformH LCoreTC ())

    , external "compareCoreAt"
        (compareCoreAtT :: TransformH LCoreTC LocalPathH
                        -> TransformH LCoreTC LocalPathH
                        -> TransformH LCoreTC ())

-- HERMIT.API.Shell.Externals
{-
    , external' "dumpLemma" ((\pp nm fp r w -> getLemmaByNameT nm >>> liftPrettyH (pOptions pp) (ppLemmaT pp nm) >>> dumpT fp pp r w) :: PrettyPrinter -> LemmaName -> FilePath -> String -> Int -> TransformH LCoreTC ())
        [ "Dump named lemma to a file."
        , "dumpLemma <pretty-printer> <lemma-name> <filename> <renderer> <width>" ]
-}
    ]


------------------------------------------------------------------------------

{-
instance External (PrettyH LCore) where
  parseExternals =
    [ external' "showLemma" ((flip showLemmaT) :: PrettyPrinter -> LemmaName -> PrettyH LCore)
        [ "Display a lemma." ]
    ]
-}

