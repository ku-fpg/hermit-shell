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

    tid <- forkIO $ scottyT 3000 runWebM runAction $ do
        middleware logStdoutDev
        post "/connect"  $ connect passInfo skernel initAST
--        post "/send"     $ send 
        -- 
        post "/command"    command
        post "/display"  $ command' (performTypedEffectH "GHCi" $ ShellEffectH $ CLSModify $ showWindowAlways Nothing)
        get  "/commands"   commands
        post "/history"    history
        post "/complete"   complete

    writeFile "GenerateMe.hs" fileContents
    writeFile ".ghci" $ unlines
        [":load GenerateMe.hs"
        , "t <- connect"
        , ":def send \\cmd -> GenerateMe.send t cmd >> return \"\""
        ]
    callProcess "ghc" ["--interactive"]
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
    , "send :: Token -> String -> IO ()"
    , "send t cmd = do"
    , "  r <- post \"http://localhost:3000/command\" $ toJSON $ Command t cmd (Just 80)"
    , "  print r"
    , ""
    , "disp :: Token -> String -> IO ()"
    , "disp t cmd = do"
    , "  r <- post \"http://localhost:3000/display\" $ toJSON $ Command t cmd (Just 80)"
    , "  print r"
    , ""
    , "connect :: IO Token"
    , "connect = do"
    , "  r <- asJSON =<< post \"http://localhost:3000/connect\" (\"\" :: ByteString)"
    , "  return $ r ^. responseBody"
    ]
