{-# LANGUAGE LambdaCase #-}
module Main where

import HERMIT.Driver

import System.Environment

import Data.List
import Data.List.Split
import Data.Version
import Paths_hermit_ghci as P

hermitGHCIVersion :: String
hermitGHCIVersion = "HERMIT-GHCi v" ++ showVersion P.version

hermitGHCIUsage :: IO ()
hermitGHCIUsage = mapM_ putStrLn [hermitGHCIVersion, "", replace "hermit" "hermit-ghci" usageOutput]
    where replace :: String -> String -> String -> String
          replace old new = intercalate new . splitOn old

main :: IO ()
main = getArgs >>= \case
    (file_nm:rest) -> do
        putStrLn $ "[starting " ++ hermitGHCIVersion ++ " on " ++ file_nm ++ "]"
        hermitDriver $ file_nm : "-opt=HERMIT.GHCI" : rest
    [] -> hermitGHCIUsage
