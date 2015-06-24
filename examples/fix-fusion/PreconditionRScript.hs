module PreconditionRScript where
import HERMIT.API

preconditionR :: Rewrite LCore
preconditionR = do
  unfoldWith "h"
  >>> caseElim

