module Main
  ( main,
  )
where

import           Bank.Entrypoint        (EffectTypes, ElmTypes, runApp)
import           Bank.InMemory          (runUserServiceInMemory, runAccountServiceInMemory)
import           Bank.Jwt               (runRealJwt)
import           Control.Concurrent     (forkIO, killThread)
import           Data.Aeson             as A
import           Data.Map.Strict        as M (empty)
import           Elm                    (defaultSettings, generateElm)
import           Jose.Jwa               as J
import           Jose.Jwk               as J
import           Jose.Jwt               as J
import           Polysemy               as P
import           Polysemy.Embed         (runEmbedded)
import           Relude
import           System.Console.CmdArgs
import           System.Environment     (getEnvironment)
import           System.FilePath        ((</>))
import           System.IO              (hClose)
import           System.Process.Typed
import           System.Signal          (installHandler, sigINT, sigTERM)
import           Web.Browser

data StartArgs = StartArgs
  { server_port     :: Int
  , frontend_port   :: Int
  , frontend_folder :: String
  , key_file        :: String
  }
  deriving stock (Show, Data, Typeable)

startArgs :: StartArgs
startArgs =
  StartArgs
    { server_port = 8081 &= typ "PORT" &= help "Port of the API server"
    , frontend_port = 8080 &= typ "PORT" &= help "Port of the frontend"
    , frontend_folder = "frontend" &= typDir &= help "Folder for the frontend"
    , key_file = "secrets/jwk.sig" &= typFile &= help "Location of JWK key file, needs to be RS512"
    }
    &= summary "Event Bank"

startFrontend :: String -> Int -> MVar a -> IO ()
startFrontend frontendFolder frontendPort endFrontend = do
  oldEnv <- getEnvironment

  let command =
        setEnv (("PORT", show frontendPort) : oldEnv) $
        setStdout inherit $
        setStderr inherit $
        setStdin nullStream $
        setWorkingDir frontendFolder $
        shell "npm run start -- --no-browser"

  withProcessTerm command $ \_ -> do
    void $ takeMVar endFrontend

runServer :: FilePath -> IO (Sem EffectTypes a -> IO a)
runServer key_file = do
  jwkParseResult <- first toText <$> A.eitherDecodeFileStrict' @J.Jwk key_file

  let keys :: [J.Jwk]
      keys = [either (error . ("Invalid JWK file: " <>)) id jwkParseResult]
      encoding :: J.JwtEncoding
      encoding = J.JwsEncoding J.RS512

  userStorage <- newTVarIO M.empty
  accountStorage <- newTVarIO M.empty

  return $ P.runM
    . runRealJwt keys encoding
    . runEmbedded atomically
    . runAccountServiceInMemory accountStorage
    . runUserServiceInMemory userStorage
    . raise2Under @(Embed STM)

main :: IO ()
main = do
  StartArgs{server_port, frontend_port, frontend_folder, key_file} <- cmdArgs startArgs
  generateElm @ElmTypes $ defaultSettings (frontend_folder </> "src") ["Generated"]

  frontendMVar <- newEmptyMVar

  putTextLn "Starting frontend..."
  frontendThread <- forkIO $ startFrontend frontend_folder frontend_port frontendMVar

  initializedRunner <- runServer key_file

  putTextLn "Starting backend..."
  scottyThread <- forkIO $ runApp server_port frontend_port $ initializedRunner

  let terminate :: Bool -> IO ()
      terminate goodTerminate = do
        putMVar frontendMVar ()
        killThread frontendThread
        killThread scottyThread
        hClose stdin

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

  void $ openBrowser $ "http://localhost:" <> show server_port
  putStrLn $ "Open combined frontend under: http://localhost:" <> show server_port

  loop
