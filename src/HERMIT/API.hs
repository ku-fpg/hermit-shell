module HERMIT.API where
        
import HERMIT.GHCI.Session
        
-- | redisplays the current state to STDOUT.
display :: IO ()
display = shellEffect Display
        

-- Not exported, but useful

newtype ShellEffect :: * where
  ShellEffect :: Value -> ShellEffect a

display' :: ShellEffect
display' = ShellEffect $ prim (mkName "HERMIT.display") $ []

newtype TypedEffectH :: * -> * where
  TypedEffectH :: Value -> TypedEffectH a     

shellEffect :: ShellEffect -> TypedEffectH
shellEffect = print 'ShellEffectH

        
prim :: Name -> [Value] -> Value
prim = undefined
