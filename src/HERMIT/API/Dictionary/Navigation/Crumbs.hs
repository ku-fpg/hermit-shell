{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Navigation.Crumbs where

import HERMIT.API.Types

-- | Descend into the program within a module.
prog :: Crumb
prog = Crumb $ method "prog" []

-- | Descend into the first binding group in a program.
progHead :: Crumb
progHead = Crumb $ method "progHead" []

-- | Descend into the tail of the program.
progTail :: Crumb
progTail = Crumb $ method "progTail" []

-- | Descend into the right-hand side of a non-recursive binding.
nonrecRhs :: Crumb
nonrecRhs = Crumb $ method "nonrecRhs" []

-- | Descend into the (n-1)th definition in a recursive binding group.
recDef :: Crumb
recDef = Crumb $ method "recDef" []

-- | Descend into the right-hand side of a recursive definition.
defRhs :: Crumb
defRhs = Crumb $ method "defRhs" []

-- | Descend into the function in an application.
appFun :: Crumb
appFun = Crumb $ method "appFun" []

-- | Descend into the argument in an application.
appArg :: Crumb
appArg = Crumb $ method "appArg" []

-- | Descend into the body of a lambda.
lamBody :: Crumb
lamBody = Crumb $ method "lamBody" []

-- | Descend into the binding group of a let expression.
letBind :: Crumb
letBind = Crumb $ method "letBind" []

-- | Descend into the body of a let expression.
letBody :: Crumb
letBody = Crumb $ method "letBody" []

-- | Descend into the scrutinised expression in a case expression.
caseExpr :: Crumb
caseExpr = Crumb $ method "caseExpr" []

-- | Descend into the type of a case expression.
caseType :: Crumb
caseType = Crumb $ method "caseType" []

-- | Descend into the (n-1)th alternative in a case expression.
caseAlt :: Crumb
caseAlt = Crumb $ method "caseAlt" []

-- | Descend into the expression in a cast.
castExpr :: Crumb
castExpr = Crumb $ method "castExpr" []

-- | Descend into the coercion in a cast.
castCo :: Crumb
castCo = Crumb $ method "castCo" []

-- | Descend into the expression in a tick.
tickExpr :: Crumb
tickExpr = Crumb $ method "tickExpr" []

-- | Descend into the right-hand side of a case alternative.
altRhs :: Crumb
altRhs = Crumb $ method "altRhs" []

-- | Descend into the type within a type expression.
type_ :: Crumb
type_ = Crumb $ method "type_" []

-- | Descend into the coercion within a coercion expression.
coercion :: Crumb
coercion = Crumb $ method "coercion" []

-- | Descend into the type function in a type application.
appTyFun :: Crumb
appTyFun = Crumb $ method "appTyFun" []

-- | Descend into the type argument in a type application.
appTyArg :: Crumb
appTyArg = Crumb $ method "appTyArg" []

-- | Descend into the (n-1)th argument of a type constructor application.
tyConArg :: Crumb
tyConArg = Crumb $ method "tyConArg" []

-- | Descend into the domain of a function type.
funDom :: Crumb
funDom = Crumb $ method "funDom" []

-- | Descend into the codomain of a function type.
funCod :: Crumb
funCod = Crumb $ method "funCod" []

-- -- | Descend into the codomain of a function type.
-- forallBody :: Crumb
-- forallBody = Crumb $ method "forallBody" []

-- | Descend into the (n-1)th argument of a type constructor coercion.
reflType :: Crumb
reflType = Crumb $ method "reflType" []

-- | Descend into the function of a coercion application.
coConArg :: Crumb
coConArg = Crumb $ method "coConArg" []

-- | Descend into the coercion function in a coercion application.
appCoFun :: Crumb
appCoFun = Crumb $ method "appCoFun" []

-- | Descend into the coercion argument in a coercion application.
appCoArg :: Crumb
appCoArg = Crumb $ method "appCoArg" []

-- | Descend into the body of a forall coercion.
coForallBody :: Crumb
coForallBody = Crumb $ method "coForallBody" []

-- | Descend into the (n-1)th argument of a coercion axiom instantiation.
axiomInst :: Crumb
axiomInst = Crumb $ method "axiomInst" []

-- | Descend into the left-hand type of an unsafe coercion.
unsafeLeft :: Crumb
unsafeLeft = Crumb $ method "unsafeLeft" []

-- | Descend into the right-hand type of an unsafe coercion.
unsafeRight :: Crumb
unsafeRight = Crumb $ method "unsafeRight" []

-- | Descend into the coercion within a symmetric coercion.
symCo :: Crumb
symCo = Crumb $ method "symCo" []

-- | Descend into the left-hand type of a transitive coercion.
transLeft :: Crumb
transLeft = Crumb $ method "transLeft" []

-- | Descend into the right-hand type of a transitive coercion.
transRight :: Crumb
transRight = Crumb $ method "transRight" []

-- | Descend into the coercion within an nth projection coercion.
nthCo :: Crumb
nthCo = Crumb $ method "nthCo" []

-- | Descend into the coercion within a coercion instantiation.
instCo :: Crumb
instCo = Crumb $ method "instCo" []

-- | Descend into the type within a coercion instantiation.
instType :: Crumb
instType = Crumb $ method "instType" []

-- | Descend into the coercion within a left/right projection coercion.
lrCo :: Crumb
lrCo = Crumb $ method "lrCo" []

-- -- | Descend into the clause of a quantified clause.
-- forallBody :: Crumb
-- forallBody = Crumb $ method "forallBody" []

-- | Descend into left-hand side of a conjunction.
conjLhs :: Crumb
conjLhs = Crumb $ method "conjLhs" []

-- | Descend into right-hand side of a conjunction.
conjRhs :: Crumb
conjRhs = Crumb $ method "conjRhs" []

-- | Descend into left-hand side of a disjunction.
disjLhs :: Crumb
disjLhs = Crumb $ method "disjLhs" []

-- | Descend into right-hand side of a disjunction.
disjRhs :: Crumb
disjRhs = Crumb $ method "disjRhs" []

-- | Descend into antecedent of an implication.
antecedent :: Crumb
antecedent = Crumb $ method "antecedent" []

-- | Descend into consequent of an implication.
consequent :: Crumb
consequent = Crumb $ method "consequent" []

-- | Descend into left-hand side of an equivalence.
eqLhs :: Crumb
eqLhs = Crumb $ method "eqLhs" []

-- | Descend into right-hand side of an equivalence.
eqRhs :: Crumb
eqRhs = Crumb $ method "eqRhs" []
