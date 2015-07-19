{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.ShellEffect where

import           Control.Applicative

import           Data.Aeson
import           Data.Typeable (Proxy(..))

import           HERMIT.Shell.ShellEffect
import           HERMIT.Shell.Types
import           HERMIT.Shell.Dictionary
import           HERMIT.Shell.Proof
import           HERMIT.PrettyPrinter.Common

import           HERMIT.RemoteShell.Orphanage()

import           HERMIT.Server.Parser.Utils
import           HERMIT.Server.Parser.Name ()


import           Control.Monad.State (modify)
import           HERMIT.Shell.Externals

import qualified Data.Map as M
import           Data.List (intercalate)

-- | 'parseExternalShellEffect' is the top-level parser 
-- for Shell Effects, returning the universial return type.
parseExternalShellEffect :: ExternalParser (ShellEffect Value)
parseExternalShellEffect = 
        parseToValue (Proxy :: Proxy (ShellEffect DocH))
   <|>  parseToValue (Proxy :: Proxy (ShellEffect ()))

instance External (ShellEffect DocH) where
  parseExternals = 
    [ external "display$" (CLSModify showWindow)
    ]

instance External (ShellEffect ()) where
  parseExternals =
    [ external "resume"  Resume
    , external "abort"            Abort     -- UNIX Exit
    , external "continue"         Continue  -- Shell Exit, but not HERMIT
    , external "display" (CLSModify $ printWindowAlways Nothing)
--    , external "display$" (CLSModify $ showWindow)
--  , external "navigate" (CLSModify $ modify $ \ st -> st { cl_nav = True })
    , external "setWindow" (CLSModify $ setWindow >> printWindow Nothing)
    , external "back"            (CLSModify $ versionCmd Back)
    , external "step"            (CLSModify $ versionCmd Step)
    , external "tag"             (CLSModify . versionCmd . Tag)
    , external "setPPDiffOnly" (\ bStr -> CLSModify $
        case reads bStr of
            [(b,"")] -> modify (\st -> st { cl_diffonly = b }) >> printWindow Nothing
            _        -> fail "valid arguments are True and False" )
    , external "setFailHard"    (\ bStr -> CLSModify $
        case reads bStr of
            [(b,"")] -> modify $ \ st -> st { cl_failhard = b }
            _        -> fail "valid arguments are True and False" )
    , external "setAutoCorelint" (\ bStr -> CLSModify $
        case reads bStr of
            [(b,"")] -> modify $ flip setCoreLint b
            _        -> fail "valid arguments are True and False" )
    , external "setPP"          (\ name -> CLSModify $
        case M.lookup name pp_dictionary of
            Nothing -> fail $ "List of Pretty Printers: " ++ intercalate ", " (M.keys pp_dictionary)
            Just pp -> do modify $ \ st -> setPrettyOpts (setPretty st pp) (cl_pretty_opts st) -- careful to preserve the current options
                          printWindow Nothing)
    , external "setPPWidth" (\ w -> CLSModify $ do
            modify $ \ st -> setPrettyOpts st (updateWidthOption w (cl_pretty_opts st))
            printWindow Nothing)
    , external "setPPType" (\ str -> CLSModify $
        case reads str :: [(ShowOption,String)] of
            [(opt,"")] -> do modify $ \ st -> setPrettyOpts st (updateTypeShowOption opt (cl_pretty_opts st))
                             printWindow Nothing
            _          -> fail "valid arguments are Show, Abstract, and Omit")
    , external "setPPCoercion" (\ str -> CLSModify $
        case reads str :: [(ShowOption,String)] of
            [(opt,"")] -> do modify $ \ st -> setPrettyOpts st (updateCoShowOption opt (cl_pretty_opts st))
                             printWindow Nothing
            _          -> fail "valid arguments are Show, Abstract, and Omit")
    , external "setPPUniques" (\ str -> CLSModify $
        case reads str of
            [(b,"")] -> do modify $ \ st -> setPrettyOpts st ((cl_pretty_opts st) { po_showUniques = b })
                           printWindow Nothing
            _        -> fail "valid arguments are True and False")
    , external "stopScript" (CLSModify $ setRunningScript Nothing)

    , external "proveLemma" (\nm -> CLSModify $ interactiveProof nm >> printWindow Nothing)
{-
    , external "dump" (\fp pp r w -> CLSModify (dump fp pp r w))
-}
   ]

