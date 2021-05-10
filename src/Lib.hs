module Lib (runApp, app, ElmTypes) where

import           Control.Concurrent.MVar              (modifyMVar)
import           Data.Aeson                           (FromJSON, ToJSON,
                                                       Value (..), object, (.=))
import           Elm                                  (Elm, ElmStreet (..))
import           Network.HTTP.Client                  (defaultManagerSettings,
                                                       newManager)
import           Network.HTTP.ReverseProxy            as RP
import           Network.Wai                          (Application,
                                                       mapResponseHeaders,
                                                       pathInfo)
import           Network.Wai.Middleware.RequestLogger
import qualified Web.Scotty                           as S

app' :: MVar Int -> S.ScottyM ()
app' idStore = do
  S.get "/api/example" do
    idToSend <- liftIO $ modifyMVar idStore \i -> return (i+1, i)
    S.json $ ExampleType idToSend "No Text."

app :: MVar Int -> IO Application
app = S.scottyApp . app'

runApp :: Int -> Int -> IO ()
runApp port frontendPort = do
  idStore <- newMVar 0
  manager <- newManager defaultManagerSettings

  S.scotty port do
    let proxyApp = RP.waiProxyTo
            (const $ return $ RP.WPRProxyDest (RP.ProxyDest "localhost" frontendPort))
            RP.defaultOnExc
            manager

    S.middleware logStdoutDev
    S.middleware \oldApp req respond -> case pathInfo req of
                                            ("api":_) -> oldApp req respond
                                            _         -> proxyApp req respond

    app' idStore

data ExampleType = ExampleType
  { first  :: Int,
    second :: Text
  } deriving (Generic)
    deriving (Elm, ToJSON, FromJSON) via ElmStreet ExampleType

type ElmTypes = '[ ExampleType ]
