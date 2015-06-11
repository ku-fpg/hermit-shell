{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HERMIT.GHCI.Actions
    ( initCommandLineState
    , performTypedEffect
--    , connect
--    , command
--    , command'
--    , commands
    , history
--    , complete
    ) where

-- import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad.Reader
-- import qualified Control.Monad.State.Lazy as State

import           Data.Aeson as Aeson
-- import           Data.Aeson.Types (parseMaybe)
-- import           Data.Char (isSpace)
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid
--import           Data.Text (Text)

import           HERMIT.Dictionary
-- import           HERMIT.External
import           HERMIT.Kernel
import           HERMIT.Kure

import           HERMIT.Plugin
-- import           HERMIT.Plugin.Builder
import           HERMIT.Plugin.Types

import           HERMIT.PrettyPrinter.Common (PrettyOptions, DocH)
-- import           HERMIT.PrettyPrinter.Common (po_width)

-- import           HERMIT.Shell.Command
-- import           HERMIT.Shell.Completion
import           HERMIT.Shell.Externals
import           HERMIT.Shell.Types hiding (clm)

-- import           HERMIT.GHCI.JSON
import           HERMIT.GHCI.Renderer
import           HERMIT.GHCI.Types

-- import           HERMIT.Shell.ShellEffect

import           Prelude.Compat hiding ((<$>))

import           System.IO (Handle)

import           HERMIT.Server.Parser (parseCLT)

------------------------- connecting a new user -------------------------------
{-
connect :: PassInfo -> Kernel -> AST -> ActionH ()
connect passInfo kernel ast = do
    uid <- webm $ do sync <- ask
                     liftIO $ do
                        chan <- atomically newTChan
                        cls <- mkCLState chan ast
                        mvar <- newMVar cls
                        atomically $ do
                            st <- readTVar sync
                            let k = nextKey (users st)
                                pr = PluginReader kernel passInfo
                            writeTVar sync $ st { users = Map.insert k (pr,mvar,chan) (users st) }
                            return k
    json $ Token uid ast

-- | Generate the next user id.
nextKey :: Map.Map Integer a -> Integer
nextKey m | Map.null m = 0
          | otherwise = let (k,_) = Map.findMax m in k + 1

-- | Build a default state for a new user.
-- DEAD CODE
mkCLState :: TChan (Either String [Glyph]) -> AST -> IO CommandLineState
mkCLState chan ast = do
    ps <- defPS ast
    tlv <- newTVarIO []
    return $ CommandLineState
                { cl_pstate = ps -- { ps_render = webChannel chan }
                , cl_height         = 30
                , cl_nav            = False
                , cl_window         = mempty
                , cl_externals      = shell_externals ++ externals
                , cl_scripts        = []
                , cl_running_script = Nothing
                , cl_foci           = Map.empty
                , cl_proofstack     = Map.empty
                , cl_tags           = Map.empty
                , cl_safety         = NormalSafety
                , cl_templemmas     = tlv
                , cl_failhard       = False
                , cl_diffonly       = False
                }


data ServerCommand =
     ServerCommand Aeson.Value (TMVar Aeson.Value)
-}
--------------------------- running a command ---------------------------------
{-
command :: ActionH ()
command = do
    Command (Token u ast) cmd mWidth <- jsonData

    let changeState st = let st' = maybe st (\w -> setPrettyOpts st ((cl_pretty_opts st) { po_width = w })) mWidth
                         in setCursor ast st'

    ast' <- clm u changeState $ evalScript cmd >> State.gets cl_cursor

    (_,_,c) <- webm $ viewUser u
    es <- liftIO (getUntilEmpty c)
    let (ms,gs) = partitionEithers es
    json $ CommandResponse (optionalMsg ms) (optionalAST gs) ast'

command' :: CLT IO () -> ActionH ()
command' eval = do
    Command (Token u ast) cmd mWidth <- jsonData
    -- u      :: Int      -- User Number
    -- ast    :: Int      -- AST number
    -- cmd    :: String   -- string of the command
    -- mWidth :: Just Int -- Just (width of screen)

    liftIO $ print (u,ast,cmd,mWidth)

     -- change the width of the screen (mWidth), inside the given ast # (ast).

    let changeState st = let st' = maybe st (\w -> setPrettyOpts st ((cl_pretty_opts st) { po_width = w })) mWidth
                         in setCursor ast st'

    -- Call the shell, using the (u :: user-number), and get the result ast #.

    ast' <- clm u changeState $ eval >> State.gets cl_cursor

    -- Find infomation about this user TChan
    (_,_,c) <- webm $ viewUser u
    -- read the TChan until all is read
    es <- liftIO (getUntilEmpty c)
    -- split into messages, and AST(s)?
    let (ms,gs) = partitionEithers es
    json $ CommandResponse (optionalMsg ms) (optionalAST gs) ast'
-}
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
                , cl_safety         = NormalSafety
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
    Nothing -> do
            print ("ParseCLT fail:" :: String, val)
            return Aeson.Null
    Just m -> do
        print ("sending to internal shell" :: String)
        cls0 <- atomically $ takeTMVar ref
        -- Now, add a command-specific logger
        let orig_logger = ps_render $ cl_pstate cls0
        chan <- atomically newTChan
        let cls1 = newRenderer (webChannel chan) cls0
        (r,cls2) <- runCLT plug cls1 m
        atomically $ putTMVar ref $ newRenderer orig_logger cls2
        es <- liftIO (getUntilEmpty chan)
        -- split into messages, and AST(s)?
        case r of
          Left (CLResume sast) -> do
             print ("resume" :: String, sast)
             atomically $ writeTVar lastCall $  resumeK (pr_kernel plug) sast
             return $ object [ "result" .= (), "output" .= es ]
          Left CLAbort -> do
             print ("abort" :: String)
             atomically $ writeTVar lastCall $ abortK (pr_kernel plug) 
             return $ object [ "result" .= (), "output" .= es ]
          Left (CLError e) -> do
             putStrLn e
             return Aeson.Null
          Left _exc  -> do
                  print $ ("Left _exc : " :: String)
                  return Aeson.Null
          Right val' -> return $ object [ "result" .= val', "output" .= es ]
performTypedEffect _ _ _ _ =
    fail "performTypedEffect: typed effects are expected to be unary."


newRenderer :: (Handle -> PrettyOptions -> Either String DocH -> IO ()) -> CommandLineState -> CommandLineState
newRenderer rndr cls = cls { cl_pstate = (cl_pstate cls) { ps_render = rndr } }

-- optionalAST :: [[Glyph]] -> Maybe [Glyph]
-- optionalAST [] = Nothing
-- optionalAST gs = Just (last gs)
--
-- optionalMsg :: [String] -> Maybe String
-- optionalMsg [] = Nothing
-- optionalMsg ss = Just (unlines ss)

getUntilEmpty :: TChan a -> IO [a]
getUntilEmpty chan = ifM (atomically $ isEmptyTChan chan)
                         (return [])
                         (atomically (readTChan chan) >>= flip liftM (getUntilEmpty chan) . (:))

-------------------------- get list of commands -------------------------------
{-
-- TODO: get per-user list of commands
commands :: ActionH ()
commands = json
         $ CommandList
           [ CommandInfo (externName e) (unlines $ externHelp e) (externTags e) aTys rTy
           | e <- shell_externals ++ externals
           , let (aTys, rTy) = externTypeArgResString e ]
-}
-------------------------- get version history --------------------------------

history :: ActionH ()
history = fail "unimplemented"

---------------------------- get completions ----------------------------------
{-
complete :: ActionH ()
complete = do
    Complete u cmd <- jsonData
    let (rCmd,rPrev) = break isSpace $ reverse cmd
    res <- clm u id $ completer rPrev $ reverse rCmd
    json $ Completions res
-}
