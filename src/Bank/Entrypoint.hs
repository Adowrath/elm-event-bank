module Bank.Entrypoint
  ( EffectTypes,
    runApp,
    waiApp,
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

type EffectTypes =
  '[ Data.UserService,
     Jwt.JwtAccess,
     Embed IO
   ]

app :: (Members EffectTypes r, MonadIO (Sem r)) => MyScotty r ()
app = do
  S.post "/api/auth/create" Auth.createAccount
  S.post "/api/auth/login" Auth.login
  S.post "/api/auth/refresh" Auth.refresh
  S.post "/api/auth/logout" $ Auth.authenticate Auth.logout
  S.get "/api/auth/validate" $ Auth.authenticate $ const $ answerOk True

--  S.post "/api/account/open" $ Auth.authenticate Account.open
--  S.get "/api/account" $ Auth.authenticate Account.list
--  S.get "/api/account/:id" $ Auth.authenticate Account.get
--  S.post "/api/account/:id" $ Auth.authenticate Account.update
--  S.post "/api/account/:id/close" $ Auth.authenticate Account.close

waiApp :: (Sem EffectTypes Response -> IO Response) -> IO Application
waiApp runResponse = S.scottyAppT runResponse app

runApp :: Int -> Int -> (Sem EffectTypes Response -> IO Response) -> IO ()
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

    app

type family a ++ b where
  '[] ++ bs = bs
  (a : as) ++ bs = a : as ++ bs

type ElmTypes = Auth.ElmTypes ++ Jwt.ElmTypes
