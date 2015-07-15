{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.Crumb where

import HERMIT.Core
import HERMIT.Server.Parser.Utils

instance External Crumb where
  parseExternals =
    [
-- HERMIT.API.Navigation.Crumbs
      external' "prog" 
        ModGuts_Prog
        [ "Descend into the program within a module." ]

    , external' "progHead" 
        ProgCons_Head
        [ "Descend into the first binding group in a program." ]

    , external' "progTail" 
        ProgCons_Tail
        [ "Descend into the tail of the program." ]

    , external' "nonrecRhs" 
        NonRec_RHS
        [ "Descend into the right-hand side of a non-recursive binding." ]

    , external' "recDef" 
        Rec_Def
        [ "Descend into the (n-1)th definition in a recursive binding group." ]

    , external' "defRhs" 
        Def_RHS
        [ "Descend into the right-hand side of a recursive definition." ]

    , external' "appFun" 
        App_Fun
        [ "Descend into the function in an application." ]

    , external' "appArg" 
        App_Arg
        [ "Descend into the argument in an application." ]

    , external' "lamBody" 
        Lam_Body
        [ "Descend into the body of a lambda." ]

    , external' "letBind" 
        Let_Bind
        [ "Descend into the binding group of a let expression." ]

    , external' "letBody" 
        Let_Body
        [ "Descend into the body of a let expression." ]

    , external' "caseExpr" 
        Case_Scrutinee
        [ "Descend into the scrutinised expression in a case expression." ]

    , external' "caseType" 
        Case_Type
        [ "Descend into the type of a case expression." ]

    , external' "caseAlt" 
        Case_Alt
        [ "Descend into the (n-1)th alternative in a case expression." ]

    , external' "castExpr" 
        Cast_Expr
        [ "Descend into the expression in a cast." ]

    , external' "castCo" 
        Cast_Co
        [ "Descend into the coercion in a cast." ]

    , external' "tickExpr" 
        Tick_Expr
        [ "Descend into the expression in a tick." ]

    , external' "altRhs" 
        Alt_RHS
        [ "Descend into the right-hand side of a case alternative." ]

    , external' "type_" 
        Type_Type
        [ "Descend into the type within a type expression." ]

    , external' "coercion" 
        Co_Co
        [ "Descend into the coercion within a coercion expression." ]

    , external' "appTyFun" 
        AppTy_Fun
        [ "Descend into the type function in a type application." ]

    , external' "appTyArg" 
        AppTy_Fun
        [ "Descend into the type argument in a type application." ]

    , external' "tyConArg" 
        TyConApp_Arg
        [ "Descend into the (n-1)th argument of a type constructor application."
        ]

    , external' "funDom" 
        FunTy_Dom
        [ "Descend into the domain of a function type." ]

    , external' "funCod" 
        FunTy_CoDom
        [ "Descend into the codomain of a function type." ]

    , external' "forallTyBody" 
        ForAllTy_Body
        [ "Descend into the body of a forall type." ]

    , external' "reflType" 
        Refl_Type
        [ "Descend into the (n-1)th argument of a type constructor coercion." ]

    , external' "coConArg" 
        TyConAppCo_Arg
        [ "Descend into the function of a coercion application." ]

    , external' "appCoFun" 
        AppCo_Fun
        [ "Descend into the coercion function in a coercion application." ]

    , external' "appCoArg" 
        AppCo_Arg
        [ "Descend into the coercion argument in a coercion application." ]

    , external' "coForallBody" 
        ForAllCo_Body
        [ "Descend into the body of a forall coercion." ]

    , external' "axiomInst" 
        AxiomInstCo_Arg
        [ "Descend into the (n-1)th argument of a coercion axiom instantiation."
        ]

    , external' "unsafeLeft" 
        UnsafeCo_Left
        [ "Descend into the left-hand type of an unsafe coercion." ]

    , external' "unsafeRight" 
        UnsafeCo_Right
        [ "Descend into the right-hand type of an unsafe coercion." ]

    , external' "symCo" 
        SymCo_Co
        [ "Descend into the coercion within a symmetric coercion." ]

    , external' "transLeft" 
        TransCo_Left
        [ "Descend into the left-hand type of a transitive coercion." ]

    , external' "transRight" 
        TransCo_Right
        [ "Descend into the right-hand type of a transitive coercion." ]

    , external' "nthCo" 
        NthCo_Co
        [ "Descend into the coercion within an nth projection coercion." ]

    , external' "instCo" 
        InstCo_Co
        [ "Descend into the coercion within a coercion instantiation." ]

    , external' "instType" 
        InstCo_Type
        [ "Descend into the type within a coercion instantiation." ]

    , external' "lrCo" 
        LRCo_Co
        [ "Descend into the coercion within a left/right projection coercion." ]

    , external' "forallBody" 
        Forall_Body
        [ "Descend into the clause of a quantified clause." ]

    , external' "conjLhs" 
        Conj_Lhs
        [ "Descend into left-hand side of a conjunction." ]

    , external' "conjRhs" 
        Conj_Rhs
        [ "Descend into right-hand side of a conjunction." ]

    , external' "disjLhs" 
        Disj_Lhs
        [ "Descend into left-hand side of a disjunction." ]

    , external' "disjRhs" 
        Disj_Rhs
        [ "Descend into right-hand side of a disjunction." ]

    , external' "antecedent" 
        Impl_Lhs
        [ "Descend into antecedent of an implication." ]

    , external' "consequent" 
        Impl_Rhs
        [ "Descend into consequent of an implication." ]

    , external' "eqLhs" 
        Eq_Lhs
        [ "Descend into left-hand side of an equivalence." ]

    , external' "eqRhs" 
        Eq_Rhs
        [ "Descend into right-hand side of an equivalence." ]
    ]
