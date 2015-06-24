module PreconditionLScript where
import HERMIT.API

preconditionL :: Rewrite LCore
preconditionL = do
  unfoldWith "f"
  >>> caseElim

