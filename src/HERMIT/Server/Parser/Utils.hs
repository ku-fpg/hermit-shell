{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

#include "overlap.h"
__LANGUAGE_OVERLAPPING_INSTANCES__

module HERMIT.Server.Parser.Utils
        ( External(parseExternal, parseExternals)
        , external
        , alts
        , CmdTag(..)
        , (.+)
        ) where

import           Control.Applicative
import           Control.Monad (liftM)
import           Data.Foldable (toList)

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe, Parser)
import           Data.Text (Text, unpack, pack)

import qualified Data.HashMap.Strict as HM
import           Data.Typeable

import           HERMIT.External (CmdTag(..))
import           HERMIT.Dictionary.Navigation (Considerable(..))
import           HERMIT.Dictionary.Rules (RuleName(..))
import           HERMIT.Lemma

--import           Debug.Trace

alts :: [a -> Parser b] -> a -> Parser b
alts as a = foldr (<|>) (fail "no match") $ map ($ a) as

-----------------------------------------------

external :: External a => Text -> a -> Value -> Parser (R a)
--external nm _ _ v | traceShow ("external"::String,nm,v) False = undefined
external nm f (Object o) = case parseMaybe p o of
        Just (nm',args) | nm' == nm -> matchExternal f args
        _                           -> fail $ "no match for " ++ show nm
 where p o' = (,) <$> o' .: "method"
                  <*> o' .: "params"
external nm _ _ = fail $ "no match for " ++ show nm

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
(.+) = const

instance External Bool where
  parseExternal (Bool b) = return b
  parseExternal _        = fail "parseExternal: Bool"

instance External Int where
  parseExternal (Number n) = return $ floor n
  parseExternal _          = fail "parseExternal: Int"

instance forall g . Typeable g => External (Proxy g) where
  parseExternal (String txt) | txt ==  pack (show (typeOf (undefined :: g)))
                             = return Proxy
  parseExternal _            = fail $ "parseExternal: Proxy for " ++
                                      show (typeOf (undefined :: g))

instance External RuleName where
  parseExternal (String s) = return . RuleName $ unpack s
  parseExternal x          = fail $ "parseExternal: RuleName -- " ++ show x

instance __OVERLAPPABLE__ External e => External [e] where
  parseExternal (Array as) = mapM parseExternal $ toList as
  parseExternal _          = fail "parseExternal: Array"

instance __OVERLAPPING__ External String where
  parseExternal (String txt) = return $ unpack txt
  parseExternal _            = fail "parseExternal: String"

instance External a => External (Maybe a) where
  parseExternal Null = return Nothing
  parseExternal x    = liftM Just $ parseExternal x

instance (External a, External b) => External (Either a b) where
  parseExternal (Object (HM.lookup "Left"  -> Just x)) = fmap Left  $ parseExternal x
  parseExternal (Object (HM.lookup "Right" -> Just x)) = fmap Right $ parseExternal x
  parseExternal _ = fail "parseExternal: Either"

-----------------------------------------------------------------
instance External Considerable where
  parseExternal = parseJSON 

instance External Used where
  parseExternal = parseJSON

