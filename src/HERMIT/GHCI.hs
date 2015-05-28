{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module HERMIT.GHCI (plugin) where

import           Blaze.ByteString.Builder (fromLazyByteString)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception.Base
import           Control.Monad.Trans.Except
import           Control.Monad.Reader

import qualified Data.Aeson as Aeson
import           Data.Default

import           HERMIT.GHC hiding ((<>), liftIO)
import           HERMIT.Plugin.Builder
import           HERMIT.Plugin.Types

import           HERMIT.Kernel

import           HERMIT.GHCI.Actions
import           HERMIT.GHCI.JSON
import           HERMIT.GHCI.Types

import           HERMIT.Shell.Command
import           HERMIT.Shell.ShellEffect
import           HERMIT.Shell.Types

import           Network.HTTP.Types (Status, status200, status500)
import qualified Network.Wai as Wai
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           System.Process

import           Web.Scotty.Trans
import           Control.Monad.Remote.JSON

------------------------------- the plugin ------------------------------------

plugin :: Plugin
plugin = buildPlugin $ \ store passInfo -> if passNum passInfo == 0
                                           then hermitKernel store "front end" . server passInfo
                                           else const return

-- | The meat of the plugin, which implements the actual Web API.
server :: PassInfo -> [CommandLineOption] -> Kernel -> AST -> IO ()
server passInfo _opts skernel initAST = do
    sync <- newTVarIO def

    let -- Functions required by Scotty to run our custom WebM monad.
        response :: WebM a -> IO (Either WebAppException a)
        response = flip runReaderT sync . runExceptT . runWebT

        runWebM :: WebM a -> IO a
        runWebM m = do
            r <- response m
            case r of
                Left err -> fail $ "Startup error: " ++ show err
                Right r' -> return r'

        runAction :: WebM Wai.Response -> IO Wai.Response
        runAction m = do
            r <- response m
            case r of
                Left err -> handleError skernel err
                Right r' -> return r'


    cls <- initCommandLineState initAST
    clsVar <- atomically $ newTMVar cls
 
    let pr = PluginReader skernel passInfo    

    let fns = router
            [("send", performTypedEffect pr clsVar)
            ]
    
    let jsonRpc :: ActionH ()
        jsonRpc = do
                d <- jsonData
                liftIO $ print d
                r <- liftIO $ fns d
                liftIO $ print r
                case r of
                  Nothing -> return ()
                  Just v -> json v

    tid <- forkIO $ scottyT 3000 runWebM runAction $ do
        middleware logStdoutDev
        post "/" $ jsonRpc
        
    writeFile ".ghci-hermit" $ unlines
        ["import HERMIT.API"
        ,":set prompt \"HERMIT> \""
        ,"send welcome" -- welcome message (interactive only)
        ,"send display" -- where am I (interactive only)
        ]
    callProcess "ghc" 
        ["--interactive"
        , "-ghci-script=.ghci-hermit"
        ,"-XOverloadedStrings"
        ,"-interactive-print=HERMIT.API.printRepl"
        ]

    -- What and Why?
    throwTo tid UserInterrupt

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
                                     $ fromLazyByteString $ Aeson.encode $ Msg msg

fileContents :: String
fileContents = unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , "module GenerateMe where"
    , ""
    , "import Control.Lens"
    , "import Data.Aeson (toJSON)"
    , "import Data.Aeson.Lens"
    , "import Data.ByteString.Lazy (ByteString)"
    , "import HERMIT.GHCI.JSON"
    , "import Network.Wreq"
    , ""
    , "import HERMIT.API"
    , ""
    ]


data ServerCommand = 
     ServerCommand Aeson.Value (TMVar Aeson.Value)