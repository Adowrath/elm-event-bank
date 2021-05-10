module Main
  ( main,
  )
where

import           Control.Concurrent     (forkIO, killThread)
import           Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Elm                    (defaultSettings, generateElm)
import           Lib                    (ElmTypes, runApp)
import           System.Console.CmdArgs
import           System.Environment     (getEnvironment)
import           System.FilePath        ((</>))
import           System.Process.Typed
import           System.Signal          (installHandler, sigINT, sigTERM)

data StartArgs = StartArgs
  { server_port     :: Int
  , frontend_port   :: Int
  , frontend_folder :: String
  }
  deriving (Show, Data, Typeable)

startArgs :: StartArgs
startArgs =
  StartArgs
    { server_port = 8081 &= help "Port of the API server"
    , frontend_port = 8080 &= help "Port of the frontend"
    , frontend_folder = "frontend/src" &= help "Folder for frontend source files"
    }
    &= summary "Event Bank"

generatePortData :: String -> Int -> IO ()
generatePortData frontendFolder apiPort = TIO.writeFile (frontendFolder </> "Api" </> "Port.elm") (T.unlines modDefinition)
  where
    modDefinition :: [Text]
    modDefinition =
      [ "module Api.Port exposing (apiPort)"
      , ""
      , "apiPort : Int"
      , "apiPort = " <> show apiPort
      ]

startFrontend :: String -> Int -> MVar a -> IO ()
startFrontend frontendFolder frontendPort endFrontend = do
  oldEnv <- getEnvironment

  let command =
        setEnv (("PORT", show frontendPort) : oldEnv) $
        setStdout inherit $
        setStderr inherit $
        setStdin nullStream $
        setDelegateCtlc True $
        setWorkingDir frontendFolder $
        shell "npm run start -- --no-browser"

  withProcessTerm command $ \_ -> do
    void $ takeMVar endFrontend

main :: IO ()
main = do
  StartArgs{server_port, frontend_port, frontend_folder} <- cmdArgs startArgs
  generateElm @ElmTypes $ defaultSettings frontend_folder ["Api"]
  generatePortData frontend_folder server_port

  frontendMVar <- newEmptyMVar

  putTextLn "Starting frontend..."
  frontendThread <- forkIO $ startFrontend frontend_folder frontend_port frontendMVar

  putTextLn "Starting backend..."
  scottyThread <- forkIO $ runApp server_port

  let terminate :: Bool -> IO ()
      terminate goodTerminate = do
        putMVar frontendMVar ()
        killThread frontendThread
        killThread scottyThread

        putStrLn "Goodbye!"
        if goodTerminate then exitSuccess else exitFailure

      loop :: IO ()
      loop = do
        -- TODO Telling frontend to die used to work. Now without Ctrl-c it would leave npm running.
        -- putText "Type exit to stop: "
        -- hFlush stdout
        -- line <- getLine
        void getLine

        -- when (line == "exit") $ throwIO UserInterrupt

        loop

  installHandler sigTERM $ const (terminate False)
  installHandler sigINT $ const (terminate False)

  loop
