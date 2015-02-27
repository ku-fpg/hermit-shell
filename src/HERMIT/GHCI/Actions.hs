{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module HERMIT.GHCI.Actions
    ( connect
    , command
    , commands
    , history
    , complete
    ) where

import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad.Trans.Except
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy as State

import           Data.Char (isSpace)
import           Data.Either
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid

import           HERMIT.Dictionary
import           HERMIT.External
import           HERMIT.Kernel
import           HERMIT.Kure
import           HERMIT.Parser

import           HERMIT.Plugin
import           HERMIT.Plugin.Builder
import           HERMIT.Plugin.Types

import           HERMIT.PrettyPrinter.Common (po_width)

import           HERMIT.Shell.Command
import           HERMIT.Shell.Externals
import           HERMIT.Shell.Types hiding (clm)

import           HERMIT.GHCI.JSON
import           HERMIT.GHCI.Renderer
import           HERMIT.GHCI.Types

import           Web.Scotty.Trans

------------------------- connecting a new user -------------------------------

connect :: PassInfo -> Kernel -> AST -> ActionH ()
connect passInfo kernel ast = do
    uid <- webm $ do sync <- ask
                     liftIO $ do
                        chan <- atomically newTChan
                        cls <- mkCLState chan passInfo kernel ast
                        mvar <- newMVar cls
                        atomically $ do
                            st <- readTVar sync
                            let k = nextKey (users st)
                            writeTVar sync $ st { users = Map.insert k (mvar,chan) (users st) }
                            return k
    json $ Token uid ast

-- | Generate the next user id.
nextKey :: Map.Map Integer a -> Integer
nextKey m | Map.null m = 0
          | otherwise = let (k,_) = Map.findMax m in k + 1

-- | Build a default state for a new user.
mkCLState :: TChan (Either String [Glyph]) -> PassInfo -> Kernel -> AST -> IO CommandLineState
mkCLState chan passInfo kernel ast = do
    ps <- defPS ast kernel passInfo
    return $ CommandLineState
                { cl_pstate = ps { ps_render = webChannel chan }
                , cl_height         = 30
                , cl_nav            = False
                , cl_window         = mempty
                , cl_externals      = shell_externals ++ externals
                , cl_scripts        = []
                , cl_running_script = Nothing
                , cl_foci           = Map.empty
                , cl_proofstack     = Map.empty
                , cl_tags           = Map.empty
                }

--------------------------- running a command ---------------------------------

command :: ActionH ()
command = do
    Command (Token u ast) cmd mWidth <- jsonData

    let changeState st = let st' = maybe st (\w -> setPrettyOpts st ((cl_pretty_opts st) { po_width = w })) mWidth
                         in setCursor ast st'

    ast' <- clm u changeState $ evalScript interpShellCommand cmd >> State.gets cl_cursor

    es <- webm $ liftM snd (viewUser u) >>= liftIO . getUntilEmpty
    let (ms,gs) = partitionEithers es
    json $ CommandResponse (optionalMsg ms) (optionalAST gs) ast'

optionalAST :: [[Glyph]] -> Maybe [Glyph]
optionalAST [] = Nothing
optionalAST gs = Just (last gs)

optionalMsg :: [String] -> Maybe String
optionalMsg [] = Nothing
optionalMsg ss = Just (unlines ss)

getUntilEmpty :: TChan a -> IO [a]
getUntilEmpty chan = ifM (atomically $ isEmptyTChan chan)
                         (return [])
                         (atomically (readTChan chan) >>= flip liftM (getUntilEmpty chan) . (:))

-------------------------- get list of commands -------------------------------

-- TODO: get per-user list of commands
commands :: ActionH ()
commands = json
         $ CommandList
           [ CommandInfo (externName e) (unlines $ externHelp e) (externTags e) aTys rTy
           | e <- shell_externals ++ externals
           , let (aTys, rTy) = externTypeArgResString e ]

-------------------------- get version history --------------------------------

history :: ActionH ()
history = do
    Token u _ <- jsonData
    (cur,(k,tags)) <- clm u id $ State.gets (cl_cursor &&& cl_kernel &&& cl_tags)
    all_asts <- listK k
    json $ History [ HCmd from (fromMaybe "-- missing command!" msg) to | (to, msg, Just from) <- all_asts ]
                   [ HTag tag ast | (ast,ts) <- Map.toList tags, tag <- ts ]

---------------------------- get completions ----------------------------------

complete :: ActionH ()
complete = do
    Complete u cmd <- jsonData
    let (rCmd,rPrev) = break isSpace $ reverse cmd
    res <- clm u id $ shellComplete rPrev $ reverse rCmd
    json $ Completions res
