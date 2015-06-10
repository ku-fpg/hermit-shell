module Main (main) where

import HERMIT.GHCI.Driver (hermitShellDriver)
import System.Environment

main :: IO ()
main = getArgs >>= hermitShellDriver
