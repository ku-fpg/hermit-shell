{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs, RankNTypes, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable #-}
module HERMIT.Server.Parser.Utils 
        ( External(parseExternal, parseExternals)
        , external
        , alts
        , CmdTag(..)
        , (.+)
        ) where

import           Control.Applicative
import           Data.Foldable (toList)

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe, Parser)
import           Data.Text (Text, unpack, pack)

import           Data.Typeable

import           HERMIT.External (CmdTag(..))

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

instance External Int where
  parseExternal (Number n) = return $ floor n

instance External String where
  parseExternal (String txt) = return $ unpack $ txt
  parseExternal _            = fail "fail: String"

instance forall g . Typeable g => External (Proxy g) where
  parseExternal (String txt) | txt ==  pack (show (typeOf (undefined :: g)))
                             = return $ Proxy
  parseExternal _            = fail $ "fail: Proxy for " ++ show (typeOf (undefined :: g))

instance External [Int] where
  parseExternal (Array as) = mapM parseExternal $ toList as

