{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module HERMIT.GHCI.Types where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad.Error.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Reader

import           Data.Default.Class
import qualified Data.Map as Map
import           Data.Text.Lazy (Text)

import           HERMIT.Kernel
import           HERMIT.Plugin.Types
import           HERMIT.PrettyPrinter.Common
import           HERMIT.PrettyPrinter.Glyphs
import           HERMIT.Shell.Types hiding (clm)
import           HERMIT.GHCI.JSON

import           HERMIT.RemoteShell.Orphanage()

import           Prelude.Compat

import           Web.Scotty.Trans

import           Data.Aeson

-- | A note about the design here:
--
-- The WebM monad uses a 'ReaderT (TVar WebAppState)' rather
-- than 'StateT WebAppState' so Scotty actions are non-blocking.
-- Using a state transformer requires global state to be synchronized
-- with an MVar, meaning every request blocks. By storing a TVar
-- in a reader, only requests that modify the TVar block.
--
-- Additionally, the map held by the TVar maps users to MVars
-- containing their own CommandLineState. This is done for two reasons:
--
--   1. Calls to /command won't block, as the TVar doesn't need to be
--      changed. Currently, only /connect blocks as it adds to the map.
--
--   2. Calls to /command by the _same user_ will block each other,
--      allowing commands to complete in order. See defn of 'clm' below.
newtype WebAppState = WebAppState
    { users :: Map.Map UserID (PluginReader, MVar CommandLineState, TChan (Either String [Glyph])) }

instance Default WebAppState where
    def = WebAppState { users = Map.empty }

data WebAppException = WAEAbort | WAEResume AST | WAEError String
    deriving (Show)

newtype WebT m a = WebT { runWebT :: ExceptT WebAppException (ReaderT (TVar WebAppState) m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar WebAppState), MonadError WebAppException)

instance MonadTrans WebT where
    lift = WebT . lift . lift

type WebM = WebT IO

type ScottyH a = ScottyT Text WebM a
type ActionH a = ActionT Text WebM a

-- The monad transformer stack is quite ridiculous at this point.
-- So here are some helpers to get things to the right place.

view :: WebM WebAppState
view = ask >>= liftIO . readTVarIO

views :: (WebAppState -> b) -> WebM b
views f = fmap f view

viewUser :: UserID -> WebM (PluginReader, MVar CommandLineState, TChan (Either String [Glyph]))
viewUser u = views users >>= maybe (throwError $ WAEError "User Not Found") return . Map.lookup u

-- Do something in the CLM IO monad for a given user and state modifier.
clm :: MonadTrans t => UserID -> (CommandLineState -> CommandLineState) -> CLT IO a -> t WebM a
clm u f m = lift $ do
    (pr,mvar,_) <- viewUser u
    r <- liftIO $ do s <- takeMVar mvar
                     (r,s') <- runCLT pr (f s) m
                     let (s'',r') = either (\case CLAbort         -> (s , Left WAEAbort)
                                                  CLResume   sast -> (s', Left (WAEResume sast))
                                                  CLError    err  -> (s , Left (WAEError err))
                                                  CLContinue st   -> (st, Left (WAEError "continue not supported")))
                                           ((s',) . Right) r
                     putMVar mvar s''
                     return r'
    either throwError return r

-- Do something to the web application state.
webm :: MonadTrans t => WebM a -> t WebM a
webm = lift

-- | CommandResponse
data CommandResponse = CommandResponse { crMsg :: Maybe String
                                       , crGlyphs :: Maybe Glyphs
                                       , crAst :: AST
                                       }

instance ToJSON CommandResponse where
    toJSON cr = object $ ("ast" .= crAst cr) : fromMaybeAttr "glyphs" (crGlyphs cr) ++ fromMaybeAttr "msg" (crMsg cr)

instance FromJSON CommandResponse where
    parseJSON (Object v) = CommandResponse <$> v .:? "msg" <*> v .:? "glyphs" <*> v .: "ast"
    parseJSON _          = mzero

-- | ShellResult
data ShellResult a
  = ShellResult [Glyphs] a -- When was said, what was returned
  | ShellFailure String     -- something went wrong
  | ShellException String   -- The remote HERMIT monad failed on the server with this
    deriving Show

instance FromJSON a => FromJSON (ShellResult a) where
  parseJSON (Object o) =   ShellResult <$> o .: "output"
                                       <*> o .: "result"
                      <|>  ShellFailure   <$> o .: "failure"
                      <|>  ShellException <$> o .: "exception"
                      <|> return (ShellFailure "malformed Object returned from Server")
    where
  parseJSON _ = return (ShellFailure "Object not returned from Server")

instance ToJSON a => ToJSON (ShellResult a) where
  toJSON (ShellResult gss a) = object [ "result" .= a, "output" .= gss ]
  toJSON (ShellFailure msg)  = object [ "failure" .= msg ]
  toJSON (ShellException msg)  = object [ "exception" .= msg ]
