module HERMIT.GHCI.Driver where

import HERMIT.Driver

import Data.List
import Data.List.Split
import Data.Version

import Paths_hermit_shell as P

hermitShellDriver :: [String] -> IO ()
hermitShellDriver [] = hermitShellUsage
hermitShellDriver (file_nm:rest) = do
    putStrLn $ "[starting "
             ++ hermitShellVersion
             ++ " on "
             ++ file_nm
             ++ "]"
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

hermitShellDotfile :: [String]
hermitShellDotfile = [
    "import HERMIT.API"
  , "import Prelude hiding (log)"
  , ":set prompt \"HERMIT> \""

  -- To get around an issue where the '-interactive-print' option gets reset:
  , ":def l \\s -> return $ \":load \" ++ s ++ \"\\n:set -interactive-print=HERMIT.GHCI.Printer.printForRepl\""
  , ":def r \\s -> return $ \":reload \" ++ s ++ \"\\n:set -interactive-print=HERMIT.GHCI.Printer.printForRepl\""
--   , "send welcome" -- welcome message (interactive only)a
  , "send display" -- where am I (interactive only)
--   , "setPath (rhsOf \"rev\")"
  ]

hermitShellFlags :: [String]
hermitShellFlags = [
    "--interactive"
  , "-ghci-script=.ghci-hermit"
  , "-XOverloadedStrings"
  , "-interactive-print=HERMIT.GHCI.Printer.printForRepl"
  ]
