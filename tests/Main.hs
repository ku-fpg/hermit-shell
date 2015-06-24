module Main (main) where

import System.Exit
import System.FilePath
import System.Process

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain hermitShellTests

hermitShellTests :: TestTree
hermitShellTests = testGroup "HERMIT-shell tests" $ map mkHermitShellTest testArgs

-- subdirectory names
rootDir, examples :: FilePath
rootDir  = "tests"
examples = "examples"

type HermitTestArgs = (FilePath, FilePath, FilePath)

testArgs :: [HermitTestArgs]
testArgs =
    [ ("concatVanishes", "Flatten.hs", "FlattenScript.hs")
    , ("concatVanishes", "QSort.hs"  , "QSortScript.hs")
    , ("concatVanishes", "Rev.hs"    , "RevScript.hs")
    , ("evaluation"    , "Eval.hs"   , "EvalScript.hs")
    , ("fib"           , "Fib.hs"    , "FibScript.hs")
    , ("fib-tuple"     , "Fib.hs"    , "FibScript.hs")
    , ("flatten"       , "Flatten.hs", "FlattenScript.hs")
    , ("last"          , "Last.hs"   , "LastScript.hs")
    , ("last"          , "Last.hs"   , "NewLastScript.hs")
    , ("mean"          , "Mean.hs"   , "MeanScript.hs")
    , ("nub"           , "Nub.hs"    , "NubScript.hs")
    , ("qsort"         , "QSort.hs"  , "QSortScript.hs")
    , ("reverse"       , "Reverse.hs", "ReverseScript.hs")
    , ("new_reverse"   , "Reverse.hs", "ReverseScript.hs")
    ]

mkHermitShellTest :: HermitTestArgs -> TestTree
mkHermitShellTest (dir, hs, script) =
    testCase testName runHermitShell
  where
    testName :: TestName
    testName = "Running " ++ dir </> hs

    pathp :: FilePath
    pathp = examples </> dir

    runHermitShell :: IO ()
    runHermitShell = do
        sandboxCfgPath <- readProcess "cabal" [ "exec"
                                              , "runhaskell"
                                              , rootDir </> "CabalSandboxConfig.hs"
                                              ] ""

        let cmd :: String
            cmd = unwords $    [ "("
                               , "cd"
                               , pathp
                               , ";"

                               -- TODO: This is a hack to get tests
                               --       that fail to type-check, etc
                               --       to actually fail. Replace it
                               --       with something more robust.
                               , "ghc"
                               , "-fno-code"
                               , "-XOverloadedStrings"
                               , script
                               , "&&"

                               , "cabal"
                               , sandboxCfgPath
                               , "exec"
                               , "--"
                               , "hermit-shell"
                               , hs
                               , "+Main"
                               , script
                               , "resume"
                               , ")"
                               ]

        putStrLn cmd
        (_,_,_,rHermit) <- createProcess $ shell cmd
        code <- waitForProcess rHermit

        case code of
            ExitSuccess   -> return ()
            ExitFailure i -> assertFailure $ "HERMIT-shell exited with error " ++ show i
