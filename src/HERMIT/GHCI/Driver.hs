module HERMIT.GHCI.Driver where

import HERMIT.Driver

import Data.List
import Data.List.Split
import Data.Version

import Paths_hermit_shell as P

hermitShellDriver :: [String] -> IO ()
hermitShellDriver [] = hermitShellUsage
hermitShellDriver (file_nm:rest) = do
    putStrLn $ concat [
        "[starting "
      , hermitShellVersion
      , " on "
      , file_nm
      , "]"
      ]
    hermitDriver $ file_nm : "-opt=HERMIT.GHCI" : rest

hermitShellVersion :: String
hermitShellVersion = "HERMIT-shell v" ++ showVersion P.version

hermitShellUsage :: IO ()
hermitShellUsage = mapM_ putStrLn [
      hermitShellVersion
    , ""
    , replace "hermit" "hermit-shell" usageOutput
    ]
  where
    replace :: String -> String -> String -> String
    replace old new = intercalate new . splitOn old
