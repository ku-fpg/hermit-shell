{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs, ScopedTypeVariables, RankNTypes #-}
module HERMIT.API.KURE where
        
import Data.Proxy
import HERMIT.API.Types

-- | any-call (.. unfold command ..) applies an unfold command to all applications.
--   Preference is given to applications with more arguments.
anyCall :: forall g . Guts g => Rewrite g -> Rewrite g
anyCall (Transform rr) = Transform $ method "anyCall" $ [proxyToJSON (Proxy :: Proxy g) , rr]



