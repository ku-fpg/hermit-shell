{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs, RankNTypes, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable #-}
module HERMIT.Server.Parser.Utils 
        ( External(parseExternal, parseExternals)
        , external
        , alts
        , CmdTag(..)
        , (.+)
        ) where

import           Control.Applicative

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe, Parser)
import           Data.Text (Text)

import           Data.Typeable

import           HERMIT.External (CmdTag(..))

-- For the instances: To remove for 7.10
import          HERMIT.Context
import          HERMIT.Core
import          HERMIT.Kure.Universes
import          HERMIT.Monad
import          HERMIT.Shell.Command
import          Language.KURE

-- For now
import           Debug.Trace

alts :: [a -> Parser b] -> a -> Parser b
alts as a = foldr (<|>) (fail "no match") $ map ($ a) as

-----------------------------------------------

external :: External a => Text -> a -> [String] -> Value -> Parser (R a)
external nm _ _ v | traceShow ("external"::String,nm,v) False = undefined
external nm f _ (Object o) = case parseMaybe p o of
        Just (nm',args) | nm' == nm -> matchExternal f args
        _                           -> fail $ "no match for " ++ show nm
 where p o' = (,) <$> o' .: "method"
                  <*> o' .: "params"
external nm _ _ _ = fail $ "no match for " ++ show nm

class Typeable e => External e where
  type R e :: *
  type R e = e  -- default
  
  parseExternal :: Value -> Parser e
  parseExternal = alts parseExternals

  parseExternals :: [Value -> Parser e]
  parseExternals = [parseExternal]

  matchExternal :: e -> [Value] -> Parser (R e)

  default matchExternal :: e -> [Value] -> Parser e
  matchExternal e [] = return e
  matchExternal _ _ = fail "wrong number of arguments"

  {-# MINIMAL parseExternal | parseExternals #-}
  
instance (External a, External b) => External (a -> b) where
  type R (a -> b) = R b
--  typeString (Proxy :: Proxy (a -> b)) = typeString (Proxy :: Proxy a) ++ " -> " ++ typeString (Proxy :: Proxy b)

  parseExternal _ = error "can not parseExternal for function"
  matchExternal e (v:vs) = do
          a <- parseExternal v
          matchExternal (e a) vs
  matchExternal _ [] = fail "wrong number of arguments"

infixl 3 .+

(.+) :: b -> a -> b
(.+) = \ b _ -> b


-- We put *all* *hermit* and *GHC* types here. Otherwise, put beside the type

deriving instance Typeable Crumb
deriving instance Typeable HermitC
deriving instance Typeable HermitM
deriving instance Typeable LCore
deriving instance Typeable LCoreTC
deriving instance Typeable SnocPath
deriving instance Typeable Transform                    -- KURE
deriving instance Typeable TypedEffectH
