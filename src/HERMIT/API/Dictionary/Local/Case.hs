{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Local.Case where

import Data.Aeson
import HERMIT.API.Types
import HERMIT.Lemma

-- | (case ec of alt -> e) v ==> case ec of alt -> e v
caseFloatApp :: Rewrite LCore
caseFloatApp = Transform $ method "caseFloatApp" []

{-| 
  Given a proof that f is strict, then
  
  f (case s of alt -> e) ==> case s of alt -> f e
-}
caseFloatArg :: Rewrite LCore -> Rewrite LCore
caseFloatArg r = Transform $ method "caseFloatArg" 
    [toJSON (Nothing :: Maybe String), toJSON (Just r)]

{-| 
  For a specified f, given a proof that f is strict, then
  
  f (case s of alt -> e) ==> case s of alt -> f e
-}
caseFloatArgWith :: String -> Rewrite LCore -> Rewrite LCore
caseFloatArgWith nm r = Transform $ method "caseFloatArg"
    [toJSON (Just nm), toJSON (Just r)]

{-| 
  For a specified f,

  f (case s of alt -> e) ==> case s of alt -> f e
-}
caseFloatArgUnsafe :: String -> Rewrite LCore
caseFloatArgUnsafe nm = Transform $ method "caseFloatArg"
    [toJSON (Just nm), toJSON (Nothing :: Maybe (Rewrite LCore))]

-- | f (case s of alt -> e) ==> case s of alt -> f e
caseFloatArgLemmaUnsafe :: Name -> Rewrite LCore
caseFloatArgLemmaUnsafe nm = Transform $ method "caseFloatArgLemma" 
    [toJSON UnsafeUsed, toJSON nm]

-- | f (case s of alt -> e) ==> case s of alt -> f e
-- Generates a lemma with given name for strictness side condition on f.
caseFloatArgLemma :: Name -> Rewrite LCore
caseFloatArgLemma nm = Transform $ method "caseFloatArgLemma" 
    [toJSON Obligation, toJSON nm]

{-| 
  case (case ec of alt1 -> e1) of alta -> ea ==> 
  case ec of alt1 -> case e1 of alta -> ea
-}
caseFloatCase :: Rewrite LCore
caseFloatCase = Transform $ method "caseFloatCase" []

-- | cast (case s of p -> e) co ==> case s of p -> cast e co
caseFloatCast :: Rewrite LCore
caseFloatCast = Transform $ method "caseFloatCast" []

-- | let v = case ec of alt1 -> e1 in e ==> case ec of alt1 -> let v = e1 in e
caseFloatLet :: Rewrite LCore
caseFloatLet = Transform $ method "caseFloatLet" []

{-| 
  case-float = case-float-app <+ case-float-case <+ case-float-let <+ 
               case-float-cast
-}
caseFloat :: Rewrite LCore
caseFloat = Transform $ method "caseFloat" []

-- | Float in a Case whatever the context.
caseFloatIn :: Rewrite LCore
caseFloatIn = Transform $ method "caseFloatIn" []

{-| 
  Float in a Case whose alternatives are parallel applications of the same 
  function.
-}
caseFloatInArgs :: Rewrite LCore
caseFloatInArgs = Transform $ method "caseFloatInArgs" []

{-| 
   Case of Known Constructor
 
   case-reduce-datacon <+ case-reduce-literal
-}
caseReduce :: Rewrite LCore
caseReduce = Transform $ method "caseReduce" []

{-| 
  Case of Known Constructor

  case C v1..vn of C w1..wn -> e ==> let { w1 = v1 ; .. ; wn = vn } in e
-}
caseReduceDatacon :: Rewrite LCore
caseReduceDatacon = Transform $ method "caseReduceDatacon" []

{-| 
  Case of Known Constructor

  case L of L -> e ==> e
-}
caseReduceLiteral :: Rewrite LCore
caseReduceLiteral = Transform $ method "caseReduceLiteral" []

-- | Unfold the case scrutinee and then case-reduce.
caseReduceUnfold :: Rewrite LCore
caseReduceUnfold = Transform $ method "caseReduceUnfold" []

{-| 
  case-split 'x

  e ==> case x of C1 vs -> e; C2 vs -> e, where x is free in e
-}
caseSplit :: Name -> Rewrite LCore
caseSplit nm = Transform $ method "caseSplit" [toJSON nm]

{-| 
  case-split [| expr |]

  e ==> case expr of C1 vs -> e; C2 vs -> e
-}
caseSplitQQ :: String -> Rewrite LCore
caseSplitQQ str = Transform $ method "caseSplitQQ" [toJSON str]

{-| 
  Like case-split, but additionally inlines the matched constructor
  applications for all occurances of the named variable.
-}
caseSplitInline :: Name -> Rewrite LCore
caseSplitInline nm = Transform $ method "caseSplitInline" [toJSON nm]

{-| 
  Like case-split, but additionally inlines the matched constructor
  applications for all occurances of the case binder.
-}
caseSplitInlineQQ :: String -> Rewrite LCore
caseSplitInlineQQ str = Transform $ method "caseSplitInlineQQ" [toJSON str]

{-| 
   Force evaluation of a variable by introducing a case.

   case-intro-seq 'v is is equivalent to adding @(seq v)@ in the source code.
-}
caseIntroSeq :: String -> Rewrite LCore
caseIntroSeq str = Transform $ method "caseIntroSeq" [toJSON str]

-- | Eliminate a case that corresponds to a pointless seq.
caseElimSeq :: Rewrite LCore
caseElimSeq = Transform $ method "caseElimSeq" []

{-| 
  Inline the case binder as the case-alternative pattern everywhere in the case 
  alternatives.
-}
caseInlineAlternative :: Rewrite LCore
caseInlineAlternative = Transform $ method "caseInlineAlternative" []

{-| 
  Inline the case binder as the case scrutinee everywhere in the case 
  alternatives.
-}
caseInlineScrutinee :: Rewrite LCore
caseInlineScrutinee = Transform $ method "caseInlineScrutinee" []

{-| 
   Merge all case alternatives into a single default case.
   The RHS of each alternative must be the same.
   
   case s of {pat1 -> e ; pat2 -> e ; ... ; patn -> e} ==> case s of {_ -> e}
-}
caseMergeAlts :: Rewrite LCore
caseMergeAlts = Transform $ method "caseMergeAlts" []

{-| 
   A cleverer version of 'mergeCaseAlts' that first attempts to
   abstract out any occurrences of the alternative pattern using the case 
   binder.
-}
caseMergeAltsWithBinder :: Rewrite LCore
caseMergeAltsWithBinder = Transform $ method "caseMergeAltsWithBinder" []

-- | case s of w; C vs -> e ==> e if w and vs are not free in e
caseElim :: Rewrite LCore
caseElim = Transform $ method "caseElim" []

{-| 
  Eliminate a case, inlining any occurrences of the case binder as the 
  scrutinee.
-}
caseElimInlineScrutinee :: Rewrite LCore
caseElimInlineScrutinee = Transform $ method "caseElimInlineScrutinee" []

{-| 
  Eliminate a case, merging the case alternatives into a single default 
  alternative and inlining the case binder as the scrutinee (if possible).
-}
caseElimMergeAlts :: Rewrite LCore
caseElimMergeAlts = Transform $ method "caseElimMergeAlts" []

{-| 
  In the case alternatives, fold any occurrences of the case alt patterns to the
  case binder.
-}
caseFoldBinder :: Rewrite LCore
caseFoldBinder = Transform $ method "caseFoldBinder" []
