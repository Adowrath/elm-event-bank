module Lib (runApp, app, ElmTypes) where

import           Control.Concurrent.MVar as M
import           Data.Aeson              (FromJSON, ToJSON, Value (..), object,
                                          (.=))
import           Elm                     (Elm, ElmStreet (..))
import           Network.Wai             (Application, mapResponseHeaders)
import Network.Wai.Middleware.RequestLogger
import qualified Web.Scotty              as S

app' :: MVar Int -> S.ScottyM ()
app' idStore = do
  -- TODO Better middleware for CORS
  -- TODO Look into proxying frontend to unify URLs
  S.middleware \oldApp req respond -> oldApp req $ respond . mapResponseHeaders (("Access-Control-Allow-Origin", "*") :)

  S.get "/hello" do
    S.text "hello"

  S.get "/some-json" do
    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

  S.get "/api/example" do
    idToSend <- liftIO $ M.modifyMVar idStore \i -> return (i+1, i)
    S.json $ ExampleType idToSend "No Text."

app :: MVar Int -> IO Application
app = S.scottyApp . app'

runApp :: Int -> IO ()
runApp port = do
  idStore <- M.newMVar 0
  S.scotty port do
    S.middleware logStdoutDev
    app' idStore

data ExampleType = ExampleType
  { first  :: Int,
    second :: Text
  } deriving (Generic)
    deriving (Elm, ToJSON, FromJSON) via ElmStreet ExampleType

type ElmTypes = '[ ExampleType ]
