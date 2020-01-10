{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module HERMIT.GHCI.Actions
    ( initCommandLineState
    , performTypedEffect
--    , history
    ) where


import           Control.Concurrent.STM
import           Control.Monad.Reader

import           Data.Aeson as Aeson
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid

import           HERMIT.Debug (debug)
import           HERMIT.Dictionary
import           HERMIT.GHCI.JSON
import           HERMIT.Kernel
import           HERMIT.Kure

import           HERMIT.Plugin
import           HERMIT.Plugin.Types

import           HERMIT.PrettyPrinter.Common (PrettyOptions, DocH)
import           HERMIT.Shell.Externals
import           HERMIT.Shell.Types hiding (clm)
import           HERMIT.Shell.Proof (forceProofs)

import           HERMIT.GHCI.Renderer
import           HERMIT.GHCI.Types

import           System.IO (Handle)

import           HERMIT.Server.Parser (parseCLT)

initCommandLineState :: AST -> IO CommandLineState
initCommandLineState ast = do
    ps  <- defPS ast
    tlv <- newTVarIO []
    return CommandLineState
                { cl_pstate         = ps
                , cl_height         = 30
                , cl_nav            = False
                , cl_window         = mempty
                , cl_externals      = shell_externals ++ externals
                , cl_scripts        = []
                , cl_running_script = Nothing
                , cl_foci           = Map.empty
                , cl_proofstack     = Map.empty
                , cl_tags           = Map.empty
                , cl_safety         = NormalSafety -- TODO: Add an option for this
                , cl_templemmas     = tlv
                , cl_failhard       = False
                , cl_diffonly       = False
                }

{-
  'performTypedEffect' takes the Plugin Reader Data, a mutable CommandLineState,
  and return a function from JSON list to JSON.
-}
performTypedEffect :: TVar (IO ())
                   -> PluginReader
                   -> TMVar CommandLineState
                   -> [Aeson.Value]
                   -> IO Aeson.Value
performTypedEffect lastCall plug ref [val] =
  case parseCLT val of
    Left err -> do
            putStrLn ("Internal Error: Parser Failed" :: String)
            putStrLn $ pprintJSON $ val
            return $ toJSON $
              (ShellFailure ("Parse error: " ++ err) :: ShellResult ())
    Right m -> do
        when debug $ print ("sending to internal shell" :: String)
        cls0 <- atomically $ takeTMVar ref
        -- Now, add a command-specific logger
        let orig_logger = ps_render $ cl_pstate cls0
        chan <- atomically newTChan
        let cls1 = newRenderer (webChannel chan) cls0
        (r,cls2) <- runCLT plug cls1 (initProofs *> m)
        atomically $ putTMVar ref $ newRenderer orig_logger cls2
        es <- liftIO (getUntilEmpty chan)
        -- split into messages, and AST(s)?
        case r of
          Left (CLResume sast) -> do
             when debug $ print ("resume" :: String, sast)
             atomically $ writeTVar lastCall $  resumeK (pr_kernel plug) sast
             return $ object [ "result" .= (), "output" .= es ]
          Left CLAbort -> do
             when debug $ print ("abort" :: String)
             atomically $ writeTVar lastCall $ abortK (pr_kernel plug)
             return $ object [ "result" .= (), "output" .= es ]
          Left (CLError e) ->
             return $ object [ "exception" .= e ]
          Left _exc  -> do
                  when debug $ print ("Left _exc : " :: String)
                  return Aeson.Null
          Right val' -> return $ object [ "result" .= val', "output" .= es ]
performTypedEffect _ _ _ _ =
    fail "performTypedEffect: typed effects are expected to be unary."

initProofs :: (MonadCatch m, MonadIO m) => CLT m ()
initProofs = tryM () forceProofs

newRenderer :: (Handle -> PrettyOptions -> Either String DocH -> IO ())
            -> CommandLineState -> CommandLineState
newRenderer rndr cls = cls { cl_pstate = (cl_pstate cls) { ps_render = rndr } }


getUntilEmpty :: TChan a -> IO [a]
getUntilEmpty chan = ifM (atomically $ isEmptyTChan chan)
                         (return [])
                         (atomically (readTChan chan) >>= flip liftM (getUntilEmpty chan) . (:))

--history :: ActionH ()
--history = fail "unimplemented"
