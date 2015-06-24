module FStrictScript where
import HERMIT.API

fstrict :: Rewrite LCore
fstrict = do
  unfoldWith "f"
  >>> undefinedCase

