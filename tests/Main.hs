{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import Control.Monad.Compat (unless, when)

import Data.Foldable.Compat (forM_)

import Prelude.Compat

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process

import Test.Tasty (TestTree, TestName, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

main :: IO ()
main = defaultMain hermitShellTests

hermitShellTests :: TestTree
hermitShellTests = testGroup "HERMIT-shell tests" $ map mkHermitShellTest testArgs

-- subdirectory names
golden, dump, rootDir, examples :: FilePath
golden   = "golden"
dump     = "dump"
rootDir  = "tests"
examples = "examples"

fixName :: FilePath -> FilePath
fixName = map (\c -> if c == '.' then '_' else c)

type HermitTestArgs = (FilePath, FilePath, String, FilePath)

testArgs :: [HermitTestArgs]
testArgs =
    [ ("casereduce"    , "Main.hs"     , "Main"     , "CaseReduceScript.hs")
    , ("concatVanishes", "Flatten.hs"  , "Main"     , "FlattenScript.hs")
    , ("concatVanishes", "QSort.hs"    , "Main"     , "QSortScript.hs")
    , ("concatVanishes", "Rev.hs"      , "Main"     , "RevScript.hs")
    , ("evaluation"    , "Eval.hs"     , "Main"     , "EvalScript.hs")
    , ("fib"           , "Fib.hs"      , "Main"     , "FibScript.hs")
    , ("fib-tuple"     , "Fib.hs"      , "Main"     , "FibScript.hs")
    , ("fib-stream"    , "Fib.hs"      , "Main"     , "FibScript.hs")
    , ("fix-fusion"    , "Fusion.hs"   , "Main"     , "FusionScript.hs")
    , ("flatten"       , "Flatten.hs"  , "Main"     , "FlattenScript.hs")
    , ("hanoi"         , "Hanoi.hs"    , "Main"     , "HanoiScript.hs")
    , ("induction"     , "Induction.hs", "Main"     , "VerifyScript.hs")
    , ("last"          , "Last.hs"     , "Main"     , "LastScript.hs")
    , ("last"          , "Last.hs"     , "Main"     , "NewLastScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyAppendAssocScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyAppendNilScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyAppendNonemptyScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyConcatAppendScript.hs")
--    , ("laws"          , "ListLaws.hs" , "ListLaws", "VerifyConcatConcatScript.hs")
--    , ("laws"          , "ListLaws.hs" , "ListLaws", "VerifyConcatNonemptyScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyConcatOfToListScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyConcatUnitScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyListLeftUnitScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyListMonadAssocScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyMapAppendScript.hs")
--    , ("laws"          , "ListLaws.hs" , "ListLaws", "VerifyMapComposeScript.hs")
--    , ("laws"          , "ListLaws.hs" , "ListLaws", "VerifyMapConcatScript.hs")
    , ("laws"          , "ListLaws.hs" , "ListLaws" , "VerifyMapNonemptyScript.hs")
--    , ("laws"          , "ListLaws.hs" , "ListLaws", "VerifyNilAppendScript.hs")
    , ("map-fusion"    , "MapFusion.hs", "MapFusion", "MapFusionScript.hs")
    , ("mean"          , "Mean.hs"     , "Main"     , "MeanScript.hs")
    , ("new_reverse"   , "Reverse.hs"  , "Main"     , "ReverseScript.hs")
    , ("nub"           , "Nub.hs"      , "Main"     , "NubScript.hs")
    , ("qsort"         , "QSort.hs"    , "Main"     , "QSortScript.hs")
    , ("reverse"       , "Reverse.hs"  , "Main"     , "ReverseScript.hs")
    ]

mkHermitShellTest :: HermitTestArgs -> TestTree
mkHermitShellTest (dir, hs, moduleName, script) =
    goldenVsFileDiff testName diff gfile dfile hermitShellOutput
  where
    testName :: TestName
    testName = dir </> script

    fixed, gfile, dfile, pathp :: FilePath
    fixed = fixName (concat [dir, "_", hs, "_", script])
    gfile = rootDir  </> golden </> fixed <.> "ref"
    dfile = rootDir  </> dump   </> fixed <.> "dump"
    pathp = examples </> dir

    diff :: FilePath -> FilePath -> [String]
    diff ref new = ["diff", "-b", "-U 5", "-IGHCi, version", ref, new]

    withPathpDir :: String -> String
    withPathpDir cmd = unwords [ "(", "cd", pathp, ";", cmd, ")" ]

    -- For some incredibly bizarre reason, HERMIT's output can be have different
    -- line orderings depending on if it's been run once before. As far as I can
    -- tell, this is due to the presence of object (.o) and interface (.hi) files.
    -- Wat.
    --
    -- Luckily, removing any object or interface before running HERMIT seems to
    -- provide a guarantee that HERMIT's output will be the same on subsequent runs.
    cleanObjectFiles :: IO ()
    cleanObjectFiles = do
        files <- getDirectoryContents pathp
        forM_ files $ \file ->
            when (takeExtension file `elem` [".o", ".hi"]) $
               removeFile $ pathp </> file

    hermitShellOutput :: IO ()
    hermitShellOutput = do
        cleanObjectFiles
        pwd <- getCurrentDirectory

        let sandboxCfgPath :: FilePath
            sandboxCfgPath = pwd </> "cabal.sandbox.config"

        sandboxExists <- doesFileExist sandboxCfgPath

        let sandboxFlag :: String
            sandboxFlag | sandboxExists = "--sandbox-config-file=" ++ sandboxCfgPath
                        | otherwise     = ""

            -- Runs GHC's typechecker over the script file to ensure it will actually
            -- work when given to HERMIT-shell.
            typeCheck :: String
            typeCheck = withPathpDir $ unwords
                [ "cabal"
                , sandboxFlag
                , "exec"
                , "--"
                , "ghc"
                , "-fno-code"
                , "-XOverloadedStrings"
                , script
                ]

            hermitShell :: String
            hermitShell = withPathpDir $ unwords
                [ "cabal"
                , sandboxFlag
                , "exec"
                , "--"
                , "hermit-shell"
                , hs
                , '+':moduleName
                , script
                , "resume"
                , "-- -w" -- Disable warnings
                ]

        (_,_,Just stdErrH,rTypeCheck) <- createProcess (shell typeCheck) {
            std_out = CreatePipe
          , std_err = CreatePipe
        }
        code <- waitForProcess rTypeCheck

        case code of
             ExitSuccess   -> return ()
             ExitFailure i -> do
                 err <- hGetContents stdErrH
                 putStrLn err
                 error $ script ++ " failed to typecheck. Error code: " ++ show i

        -- Adding a &> dfile redirect in cmd causes the call to GHC to not block
        -- until the compiler is finished (on Linux, not OSX). So we do the Haskell
        -- equivalent here by opening our own file.
        fh <- openFile dfile WriteMode
        -- putStrLn hermitShell
        (_,_,_,rHermitShell) <- createProcess (shell hermitShell) {
            std_out = UseHandle fh
          , std_err = UseHandle fh
        }
        _ <- waitForProcess rHermitShell

        -- Ensure that the golden file exists prior to calling diff
        goldenExists <- doesFileExist gfile
        unless goldenExists $ copyFile dfile gfile
