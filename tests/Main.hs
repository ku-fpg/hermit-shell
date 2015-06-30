module Main (main) where

import Control.Monad (unless)

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
    [ ("concatVanishes", "Flatten.hs", "Main", "FlattenScript.hs")
    , ("concatVanishes", "QSort.hs"  , "Main", "QSortScript.hs")
    , ("concatVanishes", "Rev.hs"    , "Main", "RevScript.hs")
    , ("evaluation"    , "Eval.hs"   , "Main", "EvalScript.hs")
    , ("fib"           , "Fib.hs"    , "Main", "FibScript.hs")
    , ("fib-tuple"     , "Fib.hs"    , "Main", "FibScript.hs")
    , ("flatten"       , "Flatten.hs", "Main", "FlattenScript.hs")
    , ("last"          , "Last.hs"   , "Main", "LastScript.hs")
    , ("last"          , "Last.hs"   , "Main", "NewLastScript.hs")
    , ("mean"          , "Mean.hs"   , "Main", "MeanScript.hs")
    , ("nub"           , "Nub.hs"    , "Main", "NubScript.hs")
    , ("qsort"         , "QSort.hs"  , "Main", "QSortScript.hs")
    , ("reverse"       , "Reverse.hs", "Main", "ReverseScript.hs")
    , ("new_reverse"   , "Reverse.hs", "Main", "ReverseScript.hs")
    , ("fix-fusion"    , "Fusion.hs" , "Main", "FusionScript.hs")
    , ("hanoi"         , "Hanoi.hs"  , "Main", "HanoiScript.hs")
    , ("induction"     , "Induction.hs", "Main", "VerifyScript.hs")

    , ("laws"          , "ListLaws.hs", "ListLaws", "VerifyAppendNilScript.hs")
    ]

mkHermitShellTest :: HermitTestArgs -> TestTree
mkHermitShellTest (dir, hs, moduleName, script) =
    goldenVsFileDiff testName diff gfile dfile hermitShellOutput
  where
    testName :: TestName
    testName = dir </> hs

    fixed, gfile, dfile, pathp :: FilePath
    fixed = fixName (concat [dir, "_", hs, "_", script])
    gfile = rootDir  </> golden </> fixed <.> "ref"
    dfile = rootDir  </> dump   </> fixed <.> "dump"
    pathp = examples </> dir

    diff :: FilePath -> FilePath -> [String]
    diff ref new = ["diff", "-b", "-U 5", ref, new]

    withPathpDir :: String -> String
    withPathpDir cmd = unwords [ "(", "cd", pathp, ";", cmd, ")" ]

    hermitShellOutput :: IO ()
    hermitShellOutput = do
        sandboxCfgPath <- readProcess "cabal" [ "exec"
                                              , "runhaskell"
                                              , rootDir </> "CabalSandboxConfig.hs"
                                              ] ""

        -- Runs GHC's typechecker over the script file to ensure it will actually
        -- work when given to HERMIT-shell.
        let typeCheck :: String
            typeCheck = withPathpDir $ unwords
                [ "ghc"
                , "-fno-code"
                , "-XOverloadedStrings"
                , script
                ]

            hermitShell :: String
            hermitShell = withPathpDir $ unwords
                [ "cabal"
                , sandboxCfgPath
                , "exec"
                , "--"
                , "hermit-shell"
                , hs
                , '+':moduleName
                , script
                , "resume"
                ]

        (_,Just _,Just stdErrH,rTypeCheck) <- createProcess (shell typeCheck) {
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

--         putStrLn hermitShell
--         (_,_,_,rHermit) <- createProcess $ shell hermitShell
--         code <- waitForProcess rHermit
--
--         case code of
--             ExitSuccess   -> return ()
--             ExitFailure i -> assertFailure $ "HERMIT-shell exited with error " ++ show i
