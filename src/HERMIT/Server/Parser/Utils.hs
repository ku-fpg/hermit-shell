{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
module HERMIT.Server.Parser.Utils 
        ( External(..)
        , external
        , alts
        , CmdTag(..)
        , (.+)
        ) where

import           Control.Applicative

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe, Parser)
import           Data.Text (Text)

import           HERMIT.External (CmdTag(..))

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

class External e where
  type R e :: *
  type R e = e  -- default
  
  parseExternal :: Value -> Parser e

  matchExternal :: e -> [Value] -> Parser (R e)

  default matchExternal :: e -> [Value] -> Parser e
  matchExternal e [] = return e
  matchExternal _ _ = fail "wrong number of arguments"
  
instance (External a, External b) => External (a -> b) where
  type R (a -> b) = R b
  parseExternal _ = error "can not parseExternal for function"
  matchExternal e (v:vs) = do
          a <- parseExternal v
          matchExternal (e a) vs
  matchExternal _ [] = fail "wrong number of arguments"

infixl 3 .+

(.+) :: b -> a -> b
(.+) = \ b _ -> b

