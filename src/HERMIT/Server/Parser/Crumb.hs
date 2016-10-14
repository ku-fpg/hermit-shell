{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.Crumb where

import HERMIT.Core
import HERMIT.Server.Parser.Utils

instance External Crumb where
  parseExternals =
    [
-- HERMIT.API.Navigation.Crumbs
      external "prog"
        ModGuts_Prog

    , external "progHead"
        ProgCons_Head

    , external "progTail"
        ProgCons_Tail

    , external "nonrecRhs"
        NonRec_RHS

    , external "recDef"
        Rec_Def

    , external "defRhs"
        Def_RHS

    , external "appFun"
        App_Fun

    , external "appArg"
        App_Arg

    , external "lamBody"
        Lam_Body

    , external "letBind"
        Let_Bind

    , external "letBody"
        Let_Body

    , external "caseExpr"
        Case_Scrutinee

    , external "caseType"
        Case_Type

    , external "caseAlt"
        Case_Alt

    , external "castExpr"
        Cast_Expr

    , external "castCo"
        Cast_Co

    , external "tickExpr"
        Tick_Expr

    , external "altRhs"
        Alt_RHS

    , external "type_"
        Type_Type

    , external "coercion"
        Co_Co

    , external "appTyFun"
        AppTy_Fun

    , external "appTyArg"
        AppTy_Fun

    , external "tyConArg"
        TyConApp_Arg

    , external "funDom"
        FunTy_Dom

    , external "funCod"
        FunTy_CoDom

    , external "forallTyBody"
        ForAllTy_Body

    , external "reflType"
        Refl_Type

    , external "coConArg"
        TyConAppCo_Arg

    , external "appCoFun"
        AppCo_Fun

    , external "appCoArg"
        AppCo_Arg

#if __GLASGOW_HASKELL__ > 710
    , external "forallCoKindCo"
        ForAllCo_KindCo
    , external "forallCoCo"
        ForAllCo_Co
#else
    , external "coForallBody"
        ForAllCo_Body
#endif

    , external "axiomInst"
        AxiomInstCo_Arg

    , external "unsafeLeft"
        UnsafeCo_Left

    , external "unsafeRight"
        UnsafeCo_Right

    , external "symCo"
        SymCo_Co

    , external "transLeft"
        TransCo_Left

    , external "transRight"
        TransCo_Right

    , external "nthCo"
        NthCo_Co

#if __GLASGOW_HASKELL__ > 710
    , external "instCoLeft"
        InstCo_Left
    , external "instCoRight"
        InstCo_Right
#else
    , external "instCo"
        InstCo_Co
    , external "instType"
        InstCo_Type
#endif

    , external "lrCo"
        LRCo_Co

    , external "forallBody"
        Forall_Body

    , external "conjLhs"
        Conj_Lhs

    , external "conjRhs"
        Conj_Rhs

    , external "disjLhs"
        Disj_Lhs

    , external "disjRhs"
        Disj_Rhs

    , external "antecedent"
        Impl_Lhs

    , external "consequent"
        Impl_Rhs

    , external "eqLhs"
        Eq_Lhs

    , external "eqRhs"
        Eq_Rhs
    ]
