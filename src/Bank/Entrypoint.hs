module Bank.Entrypoint
  ( runApp,
    app,
    ElmTypes,
  )
where

import qualified Bank.Auth                            as Auth
import qualified Bank.Data                            as Data
import qualified Bank.Jwt                             as Jwt
import           Bank.Types
import           Bank.WebHelpers
import           Network.HTTP.Client                  (defaultManagerSettings,
                                                       newManager)
import           Network.HTTP.ReverseProxy            as RP
import           Network.Wai                          (Application, Response,
                                                       pathInfo)
import           Network.Wai.Middleware.RequestLogger
import           Polysemy
import           Relude
import qualified Web.Scotty.Trans                     as S

app' :: (Members '[Data.UserData, Jwt.JwtAccess] r, MonadIO (Sem r)) => MyScotty r ()
app' = do
  S.post "/api/auth/login" Auth.login
  S.post "/api/auth/refresh" Auth.refresh
  S.post "/api/auth/logout" $ Auth.authenticate Auth.logout
  S.get "/api/auth/validate" $ Auth.authenticate $ const $ answerOk True

app :: (Sem '[Data.UserData, Jwt.JwtAccess, Embed IO] Response -> IO Response) -> IO Application
app runResponse = S.scottyAppT runResponse app'

runApp :: Int -> Int -> (Sem '[Data.UserData, Jwt.JwtAccess, Embed IO] Response -> IO Response) -> IO ()
runApp port frontendPort runResponse = do
  manager <- newManager defaultManagerSettings

  S.scottyT port runResponse do
    let proxyApp =
          RP.waiProxyTo
            (const $ return $ RP.WPRProxyDest (RP.ProxyDest "localhost" frontendPort))
            RP.defaultOnExc
            manager

    S.middleware logStdoutDev
    S.middleware \oldApp req respond -> case pathInfo req of
      ("api" : _) -> oldApp req respond
      _           -> proxyApp req respond

    app'

type family a ++ b where
  '[] ++ bs = bs
  (a : as) ++ bs = a : as ++ bs

type ElmTypes = Auth.ElmTypes ++ Jwt.ElmTypes
