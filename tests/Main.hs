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
    ]

mkHermitShellTest :: HermitTestArgs -> TestTree
mkHermitShellTest (dir, hs, moduleName, script) =
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
                               , '+':moduleName
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
