{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HERMIT.GHCI (plugin) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception.Base
import           Control.Monad.Compat
import           Control.Monad.IO.Class
import           Control.Monad.Remote.JSON
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader

import qualified Data.Aeson as Aeson
import           Data.ByteString.Builder (lazyByteString)
import           Data.Default.Class
import           Data.List.Compat
import           Data.Maybe (maybeToList)

import           HERMIT.Debug (debug)
import           HERMIT.GHC hiding ((<>), liftIO)
import           HERMIT.Kernel
import           HERMIT.Plugin.Builder
import           HERMIT.Plugin.Types

import           HERMIT.GHCI.Actions
import           HERMIT.GHCI.JSON
import           HERMIT.GHCI.Types

import           Network.HTTP.Types (Status, status200, status500)
import qualified Network.Wai as Wai
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Prelude.Compat

import           System.FilePath (takeExtension)
import           System.IO (hPutStrLn, hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process

import           Web.Scotty.Trans

-- import           System.Exit
-- import           System.Posix.Signals

------------------------------- the plugin ------------------------------------

plugin :: Plugin
plugin = buildPlugin $ \ store passInfo ->
  if passNum passInfo == 0
  then \ o p ->
           do when debug . liftIO $ print ("Hey" :: String)
              r <- hermitKernel store "front end" (server passInfo $ reverse o) p
              when debug . liftIO $ print ("Jude" :: String)
              return r
  else const return

-- | The meat of the plugin, which implements the actual Web API.
server :: PassInfo -> [CommandLineOption] -> Kernel -> AST -> IO ()
server passInfo opts skernel initAST = do
    let mbScript :: Maybe FilePath
        mbScript = flip find opts $ (== ".hs") . takeExtension

        resume :: Bool
        resume = "resume" `elem` opts

        otherOpts :: [CommandLineOption]
        otherOpts = filter (not . (`elem` ("resume":maybeToList mbScript))) opts
    unless (null otherOpts) $ do
        putStr "Ignored command-line arguments: "
        forM_ otherOpts $ \opt -> putStr opt >> putChar ' '
        putStrLn ""

    sync' <- newTVarIO def -- TODO: is this used anywhere?

    let -- Functions required by Scotty to run our custom WebM monad.
        response :: WebM a -> IO (Either WebAppException a)
        response = flip runReaderT sync' . runExceptT . runWebT

        runAction :: WebM Wai.Response -> IO Wai.Response
        runAction m = do
            r <- response m
            case r of
                Left err -> handleError skernel err
                Right r' -> return r'


    cls <- initCommandLineState initAST
    clsVar <- atomically $ newTMVar cls

    let pr = PluginReader skernel passInfo

    lastCall <- newTVarIO (abortK (pr_kernel pr) :: IO ())

    let fns = router
            [("send", performTypedEffect lastCall pr clsVar)
            ]

    let jsonRpc :: ActionH ()
        jsonRpc = do
                d <- jsonData
                when debug . liftIO $ print d
                r <- liftIO $ fns d
--                when debug . liftIO $ print r
                forM_ r json

    tid <- forkIO $ scottyT 3000 runAction $ do
        when debug $ middleware logStdoutDev
        post "/" jsonRpc

    _code <- withSystemTempFile ".ghci-hermit" $ \fp h -> do
        hPutStrLn h $ hermitShellDotfile mbScript
        hClose h

        let ghci = (shell . unwords $ "ghci" : hermitShellFlags fp) {
            std_in = if resume then CreatePipe else Inherit
        }
        (mbStdin, _, _, hGhci) <- createProcess ghci
        when resume $ forM_ mbStdin (`hPutStrLn` ":resume")
        waitForProcess hGhci

    -- What and Why?
    when debug $ print ("Killing server" :: String)
    throwTo tid UserInterrupt
    when debug $ print ("Killed server" :: String)

    when debug $ print ("Last Call" :: String)
    join (atomically (readTVar lastCall))      -- do last call

    when debug $ print ("Last Called" :: String)
 --   raiseSignal sigTERM

-- | Turn WebAppException into a Response.
handleError :: Kernel -> WebAppException -> IO Wai.Response
handleError k WAEAbort = do
    abortK k
    return $ msgBuilder "HERMIT Aborting" status200
handleError k (WAEResume sast) = do
    resumeK k sast
    return $ msgBuilder "HERMIT Resuming" status200
handleError _ (WAEError str) = return $ msgBuilder str status500

-- | Turn a string and status into a Response containing a JSON-encoded Msg.
msgBuilder :: String -> Status -> Wai.Response
msgBuilder msg s = Wai.responseBuilder s [("Content-Type","application/json")]
    . lazyByteString . Aeson.encode $ Msg msg

hermitShellDotfile :: Maybe FilePath -> String
hermitShellDotfile mbScript = unlines $
  [ ":m HERMIT.API" -- NOTE: All other modules intentionally unimported here
  , "import Prelude hiding (log, repeat)"
  , ":set prompt \"HERMIT> \""

  -- To get around an issue where the '-interactive-print' option gets reset:
  , ":def! l \\s -> return $ \":load \" ++ s ++ \"\\n:set -interactive-print=HERMIT.GHCI.Printer.printForRepl\""
  , ":def! r \\s -> return $ \":reload \" ++ s ++ \"\\n:set -interactive-print=HERMIT.GHCI.Printer.printForRepl\""
  , ":def! hermit \\s -> return $ \":set -interactive-print=HERMIT.GHCI.Printer.printForRepl\""
  , ":def! resume \\s -> return $ \"resume\\n:quit\""
  , ":def! abort \\s -> return $ \"abort\\n:quit\""
  , ":def! doc \\s -> return $ \":!hoogle --info \" ++ show s ++ \" +hermit-shell\""
--   , "send welcome" -- welcome message (interactive only)a
  , "send display" -- where am I (interactive only)
--   , "setPath (rhsOf \"rev\")"
  ] ++ maybe []
             (\script ->
                let moduleName' = takeWhile (/='.') script
                in [":l " ++ script, moduleName' ++ ".script"])
             mbScript

hermitShellFlags :: FilePath -> [String]
hermitShellFlags dotfilePath =
  [ "--interactive"
  , "-ghci-script=" ++ dotfilePath
  , "-XOverloadedStrings"
  , "-interactive-print=HERMIT.GHCI.Printer.printForRepl"
  ]

-- fileContents :: String
-- fileContents = unlines
--     [ "{-# LANGUAGE OverloadedStrings #-}"
--     , ""
--     , "module GenerateMe where"
--     , ""
--     , "import Control.Lens"
--     , "import Data.Aeson (toJSON)"
--     , "import Data.Aeson.Lens"
--     , "import Data.ByteString.Lazy (ByteString)"
--     , "import HERMIT.GHCI.JSON"
--     , "import Network.Wreq"
--     , ""
--     , "import HERMIT.API"
--     , ""
--     ]
--
--
-- data ServerCommand =
--      ServerCommand Aeson.Value (TMVar Aeson.Value)
