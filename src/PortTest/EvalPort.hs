{-# LANGUAGE OverloadedStrings #-}

-- TODO: Make sure this works with multi-line curly brace blocks

import System.Environment
import Data.Monoid ((<>))

import Data.Char (isSpace)
import Data.List

getFileName :: IO String
getFileName = do
    args <- getArgs
    case args of
        [fileName] -> return fileName
        _          -> error $ "Wrong number of arguments. Expected 1, got "
                               ++ show (length args)

preamble :: String
preamble
    = unlines
      [ "import HERMIT.API"
      , "script :: Shell ()"
      , "script = do"
      ]

convert :: String -> String
convert = unlines . map convertLine . lines

convertLine :: String -> String
convertLine ""   = ""
convertLine line = "  eval " <> show line

main :: IO ()
main = do
    fileName <- getFileName
    file <- readFile fileName

    putStrLn (preamble <> convert file)
