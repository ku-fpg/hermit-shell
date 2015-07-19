{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}


#include "overlap.h"
__LANGUAGE_OVERLAPPING_INSTANCES__

module HERMIT.Server.Parser.Utils
        ( External(parsePrimitive, parseExternals)
        , parseExternal
        , external
        , alts
        , reply
        , CmdTag(..)
        , (.+)
        ) where

import           Control.Applicative
import           Control.Monad (liftM)
import           Data.Monoid
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

newtype ExternalParser :: * -> * where
  ExternalParser     :: (Value -> Parser a) -> ExternalParser a

instance Functor ExternalParser where
  fmap f (ExternalParser g) = ExternalParser (fmap f . g)

instance Monoid (ExternalParser a) where
  mempty = ExternalParser (fail "mempty")
  mappend (ExternalParser f) (ExternalParser g) = ExternalParser $ \ v ->
          f v <|> g v

alts :: [ExternalParser b] -> ExternalParser b
alts = mconcat -- as a = foldr (<|>) (fail "no match") $ map ($ a) as

-----------------------------------------------

external :: External a => Text -> a -> ExternalParser (R a)
external nm f = ExternalParser $ \ v -> case v of
        (Object o) -> case parseMaybe p o of
                Just (nm',args) | nm' == nm -> matchExternal f args
                _                           -> fail $ "no match for " ++ show nm
        _ -> fail $ "no match for " ++ show nm
   where p o' = (,) <$> o' .: "method"
                    <*> o' .: "params"

-- convert a parser to return a JSON Value
reply :: (Functor f, ToJSON e) => ExternalParser (f e) -> ExternalParser (f Value)
reply = fmap (fmap toJSON) 

parseExternal :: External e => ExternalParser e
parseExternal = alts parseExternals 

class External e where
  type R e :: *
  type R e = e  -- default

  parsePrimitive :: Value -> Parser e
  parsePrimitive v = case alts parseExternals of
                      ExternalParser f -> f v

  parseExternals :: [ExternalParser e]
  parseExternals = [ExternalParser parsePrimitive]

  matchExternal :: e -> [Value] -> Parser (R e)

  default matchExternal :: e -> [Value] -> Parser e
  matchExternal e [] = return e
  matchExternal _ _ = fail "wrong number of arguments"

  {-# MINIMAL parsePrimitive | parseExternals #-}

instance (External a, External b) => External (a -> b) where
  type R (a -> b) = R b
--  typeString (Proxy :: Proxy (a -> b)) = typeString (Proxy :: Proxy a) ++ " -> " ++ typeString (Proxy :: Proxy b)

  parsePrimitive _ = error "can not parsePrimitive for function"
  matchExternal e (v:vs) = do
          a <- parsePrimitive v
          matchExternal (e a) vs
  matchExternal _ [] = fail "wrong number of arguments"

infixl 3 .+

(.+) :: b -> a -> b
(.+) = const

instance External Bool where
  parsePrimitive (Bool b) = return b
  parsePrimitive _        = fail "parseExternal: Bool"

instance External Int where
  parsePrimitive (Number n) = return $ floor n
  parsePrimitive _          = fail "parseExternal: Int"

instance forall g . Typeable g => External (Proxy g) where
  parsePrimitive (String txt) | txt ==  pack (show (typeOf (undefined :: g)))
                             = return Proxy
  parsePrimitive _            = fail $ "parseExternal: Proxy for " ++
                                      show (typeOf (undefined :: g))

instance External RuleName where
  parsePrimitive (String s) = return . RuleName $ unpack s
  parsePrimitive x          = fail $ "parseExternal: RuleName -- " ++ show x

instance __OVERLAPPABLE__ External e => External [e] where
  parsePrimitive (Array as) = mapM parsePrimitive $ toList as
  parsePrimitive _          = fail "parseExternal: Array"

instance __OVERLAPPING__ External String where
  parsePrimitive (String txt) = return $ unpack txt
  parsePrimitive _            = fail "parseExternal: String"

instance External a => External (Maybe a) where
  parsePrimitive Null = return Nothing
  parsePrimitive x    = liftM Just $ parsePrimitive x

instance (External a, External b) => External (Either a b) where
  parsePrimitive (Object (HM.lookup "Left"  -> Just x)) = fmap Left  $ parsePrimitive x
  parsePrimitive (Object (HM.lookup "Right" -> Just x)) = fmap Right $ parsePrimitive x
  parsePrimitive _ = fail "parseExternal: Either"

-----------------------------------------------------------------
instance External Considerable where
  parsePrimitive = parseJSON 

instance External Used where
  parsePrimitive = parseJSON

