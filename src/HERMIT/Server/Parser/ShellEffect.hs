{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.ShellEffect where

import           HERMIT.Shell.ShellEffect
import           HERMIT.Shell.Types
import           HERMIT.Shell.Dictionary
import           HERMIT.Shell.Proof
import           HERMIT.PrettyPrinter.Common

import           HERMIT.Server.Parser.Utils
import           HERMIT.Server.Parser.Name ()

import           Control.Monad.State (modify)
import           HERMIT.Shell.Externals

import qualified Data.Map as M
import           Data.List (intercalate)

instance External ShellEffect where
  parseExternals =
    [ external "resume"  Resume
        [ "stops HERMIT; resumes compile" ]
    , external "abort"            Abort     -- UNIX Exit
        [ "hard UNIX-style exit; does not return to GHC; does not save" ]
    , external "continue"         Continue  -- Shell Exit, but not HERMIT
        [ "exits shell; resumes HERMIT" ]
    , external "display" (CLSModify $ showWindowAlways Nothing)
        [ "redisplays current state" ]
--  , external "navigate" (CLSModify $ modify $ \ st -> st { cl_nav = True })
--      [ "switch to navigate mode" ]
    , external "setWindow" (CLSModify $ setWindow >> showWindow Nothing)
        [ "fix the window to the current focus" ]
    , external "back"            (CLSModify $ versionCmd Back)
        [ "go back in the derivation" ]
    , external "step"            (CLSModify $ versionCmd Step)
        [ "step forward in the derivation" ]
    , external "tag"             (CLSModify . versionCmd . Tag)
        [ "name the current step in the derivation" ]
    , external "setPPDiffOnly" (\ bStr -> CLSModify $
        case reads bStr of
            [(b,"")] -> modify (\st -> st { cl_diffonly = b }) >> showWindow Nothing
            _        -> fail "valid arguments are True and False" )
        [ "print diffs rather than full code after a rewrite" ]
    , external "setFailHard"    (\ bStr -> CLSModify $
        case reads bStr of
            [(b,"")] -> modify $ \ st -> st { cl_failhard = b }
            _        -> fail "valid arguments are True and False" )
        [ "any rewrite failure causes compilation to abort" ]
    , external "setAutoCorelint" (\ bStr -> CLSModify $
        case reads bStr of
            [(b,"")] -> modify $ flip setCoreLint b
            _        -> fail "valid arguments are True and False" )
        [ "run core lint type-checker after every rewrite, reverting on failure" ]
    , external "setPP"          (\ name -> CLSModify $
        case M.lookup name pp_dictionary of
            Nothing -> fail $ "List of Pretty Printers: " ++ intercalate ", " (M.keys pp_dictionary)
            Just pp -> do modify $ \ st -> setPrettyOpts (setPretty st pp) (cl_pretty_opts st) -- careful to preserve the current options
                          showWindow Nothing)
        [ "set the pretty printer"
        , "use 'setPP ls' to list available pretty printers" ]
    , external "setPPWidth" (\ w -> CLSModify $ do
            modify $ \ st -> setPrettyOpts st (updateWidthOption w (cl_pretty_opts st))
            showWindow Nothing)
        ["set the width of the screen"]
    , external "setPPType" (\ str -> CLSModify $
        case reads str :: [(ShowOption,String)] of
            [(opt,"")] -> do modify $ \ st -> setPrettyOpts st (updateTypeShowOption opt (cl_pretty_opts st))
                             showWindow Nothing
            _          -> fail "valid arguments are Show, Abstract, and Omit")
        ["set how to show expression-level types (Show|Abstact|Omit)"]
    , external "setPPCoercion" (\ str -> CLSModify $
        case reads str :: [(ShowOption,String)] of
            [(opt,"")] -> do modify $ \ st -> setPrettyOpts st (updateCoShowOption opt (cl_pretty_opts st))
                             showWindow Nothing
            _          -> fail "valid arguments are Show, Abstract, and Omit")
        ["set how to show coercions (Show|Abstact|Omit)"]
    , external "setPPUniques" (\ str -> CLSModify $
        case reads str of
            [(b,"")] -> do modify $ \ st -> setPrettyOpts st ((cl_pretty_opts st) { po_showUniques = b })
                           showWindow Nothing
            _        -> fail "valid arguments are True and False")
        ["set whether uniques are printed with variable names"]
    , external "stopScript" (CLSModify $ setRunningScript Nothing)
        [ "Stop running the current script." ]

    , external "proveLemma" (\nm -> CLSModify $ interactiveProof nm >> showWindow Nothing)
        [ "Proof a lemma interactively." ]

    , external "dump" (\fp pp r w -> CLSModify (dump fp pp r w))
        [ "dump <filename> <pretty-printer> <renderer> <width>" ]
   ]

