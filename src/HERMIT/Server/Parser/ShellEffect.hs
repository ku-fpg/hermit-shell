{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.ShellEffect where

import           HERMIT.Shell.ShellEffect
import           HERMIT.Shell.Types
import           HERMIT.Shell.Dictionary
import           HERMIT.Shell.ScriptToRewrite
import           HERMIT.PrettyPrinter.Common

import           HERMIT.Server.Parser.Utils

import           Control.Monad.State (modify)
import           HERMIT.Shell.Externals

import qualified Data.Map as M
import           Data.List (intercalate)

import           Data.Aeson

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
    , external "setPpDiffOnly" (\ bStr -> CLSModify $
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
    , external "setPp"          (\ name -> CLSModify $
        case M.lookup name pp_dictionary of
            Nothing -> fail $ "List of Pretty Printers: " ++ intercalate ", " (M.keys pp_dictionary)
            Just pp -> do modify $ \ st -> setPrettyOpts (setPretty st pp) (cl_pretty_opts st) -- careful to preserve the current options
                          showWindow Nothing)
        [ "set the pretty printer"
        , "use 'setPp ls' to list available pretty printers" ]
    , external "setPpWidth" (\ w -> CLSModify $ do
            modify $ \ st -> setPrettyOpts st (updateWidthOption w (cl_pretty_opts st))
            showWindow Nothing)
        ["set the width of the screen"]
    , external "setPpType" (\ str -> CLSModify $
        case reads str :: [(ShowOption,String)] of
            [(opt,"")] -> do modify $ \ st -> setPrettyOpts st (updateTypeShowOption opt (cl_pretty_opts st))
                             showWindow Nothing
            _          -> fail "valid arguments are Show, Abstract, and Omit")
        ["set how to show expression-level types (Show|Abstact|Omit)"]
    , external "setPpCoercion" (\ str -> CLSModify $
        case reads str :: [(ShowOption,String)] of
            [(opt,"")] -> do modify $ \ st -> setPrettyOpts st (updateCoShowOption opt (cl_pretty_opts st))
                             showWindow Nothing
            _          -> fail "valid arguments are Show, Abstract, and Omit")
        ["set how to show coercions (Show|Abstact|Omit)"]
    , external "setPpUniques" (\ str -> CLSModify $
        case reads str of
            [(b,"")] -> do modify $ \ st -> setPrettyOpts st ((cl_pretty_opts st) { po_showUniques = b })
                           showWindow Nothing
            _        -> fail "valid arguments are True and False")
        ["set whether uniques are printed with variable names"]
    , external "stopScript" (CLSModify $ setRunningScript Nothing)
        [ "Stop running the current script." ]
   ]

