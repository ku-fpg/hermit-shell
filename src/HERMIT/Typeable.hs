{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs, RankNTypes, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable #-}
-- Orphan instances for Typeable. Remove when we switch to 7.10 only

module HERMIT.Typeable where
        
import           Data.Typeable

import          HERMIT.Context
import          HERMIT.Core
import          HERMIT.Kure.Universes
import          HERMIT.Monad
import          HERMIT.Shell.Command
import          Language.KURE

-- We put *all* *hermit* and *KURE* types here. Otherwise, put beside the type
         
deriving instance Typeable Crumb
deriving instance Typeable HermitC
deriving instance Typeable HermitM
deriving instance Typeable LCore
deriving instance Typeable LCoreTC
deriving instance Typeable SnocPath
deriving instance Typeable Transform                    -- KURE
deriving instance Typeable TypedEffectH
