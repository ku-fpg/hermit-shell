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

-- | Descend into the argument in an application.
appArg :: Crumb

-- | Descend into the body of a lambda.
lamBody :: Crumb

-- | Descend into the binding group of a let expression.
letBind :: Crumb

-- | Descend into the body of a let expression.
letBody :: Crumb

-- | Descend into the scrutinised expression in a case expression.
caseExpr :: Crumb

-- | Descend into the type of a case expression.
caseType :: Crumb

-- | Descend into the (n-1)th alternative in a case expression.
caseAlt :: Crumb

-- | Descend into the expression in a cast.
castExpr :: Crumb

-- | Descend into the coercion in a cast.
castCo :: Crumb

-- | Descend into the expression in a tick.
tickExpr :: Crumb

-- | Descend into the right-hand side of a case alternative.
altRhs :: Crumb

-- | Descend into the type within a type expression.
type_ :: Crumb

-- | Descend into the coercion within a coercion expression.
coercion :: Crumb

-- | Descend into the type function in a type application.
appTyFun :: Crumb

-- | Descend into the type argument in a type application.
appTyArg :: Crumb

-- | Descend into the (n-1)th argument of a type constructor application.
tyConArg :: Crumb

-- | Descend into the domain of a function type.
funDom :: Crumb

-- | Descend into the codomain of a function type.
funCod :: Crumb

-- | Descend into the codomain of a function type.
forallBody :: Crumb

-- | Descend into the (n-1)th argument of a type constructor coercion.
reflType :: Crumb

-- | Descend into the function of a coercion application.
coConArg :: Crumb

-- | Descend into the coercion function in a coercion application.
appCoFun :: Crumb

-- | Descend into the coercion argument in a coercion application.
appCoArg :: Crumb

-- | Descend into the body of a forall coercion.
coForallBody :: Crumb

-- | Descend into the (n-1)th argument of a coercion axiom instantiation.
axiomInst :: Crumb

-- | Descend into the left-hand type of an unsafe coercion.
unsafeLeft :: Crumb

-- | Descend into the right-hand type of an unsafe coercion.
unsafeRight :: Crumb
symCo :: Crumb
transLeft :: Crumb
transRight :: Crumb
nthCo :: Crumb
instCo :: Crumb
instType :: Crumb
lrCo :: Crumb
forallBody :: Crumb
conjLhs :: Crumb
conjRhs :: Crumb
disjLhs :: Crumb
disjRhs :: Crumb
antecedent :: Crumb
consequent :: Crumb
eqLhs :: Crumb
eqRhs :: Crumb
