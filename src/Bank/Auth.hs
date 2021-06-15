module Bank.Auth
  ( module Bank.Auth,
  )
where

import           Bank.Data
import           Bank.Jwt
import           Bank.Types
import           Bank.WebHelpers
import           Data.Aeson                as A
import qualified Data.Text                 as T
import qualified Data.UUID.V4              as UUID (nextRandom)
import           Elm
import           GHC.TypeLits              (KnownSymbol)
import qualified Jose.Jwt                  as J
import           Network.HTTP.Types.Status (badRequest400, forbidden403,
                                            unprocessableEntity422)
import           Polysemy
import           Relude                    hiding (exp)
import           Validation                as V
import qualified Web.Scotty.Trans          as S

-------------------- COMMUNICATION OBJECTS -------------------

type ElmTypes = '[LoginData, AuthError]

data LoginData = LoginData
  { username :: Text,
    password :: Text
  }
  deriving stock (Generic)
  deriving (FromJSON, Elm) via ElmStreet LoginData

instance Validatable LoginData where
  validate l@LoginData {username, password} =
    l <$ failureIf (T.length username < 3) "Username too short - at least 3 characters"
      <* failureIf (T.length password < 8) "Password too short - at least 8 characters"

data AuthError
  = UnknownUser
  | UsernameTaken
  | WrongPassword
  | NotLoggedIn
  | SessionLoggedOut
  | NotBearerAuthenticated
  | UserNoLongerExists
  | LoginExpired
  | AuthTokenError TokenError
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet AuthError

-------------------- ROUTES -------------------

login :: (Members '[UserService, JwtAccess] r, MonadIO (Sem r)) => MyAction r ()
login = do
  LoginData {username, password} <- parseJsonBody

  user <- whenNothingM (lift $ findUser username) $ failUserError forbidden403 UnknownUser

  ifM
    (not <$> verifyPassword user password)
    (failUserError forbidden403 WrongPassword)
    (generateNewTokens user)

refresh :: (Members '[UserService, JwtAccess] r, MonadIO (Sem r)) => MyAction r ()
refresh = do
  Always SingleToken {token} <- parseJsonBody

  let rToken = RefreshToken $ extractJwtToken token

  -- Loading the user verifies the refresh token.
  (_, refreshClaims) <- loadUserFromToken rToken
  newToken <- lift $ generateNewSession refreshClaims

  answerOk newToken

logout :: Member UserService r => User -> MyAction r ()
logout user = do
  let newUser = user {refreshToken = Nothing}
  lift $ storeUser newUser
  answerOk ()

createAccount :: (Member UserService r, MonadIO (Sem r)) => MyAction r ()
createAccount = do
  LoginData {username, password} <- parseJsonBody

  whenJustM (lift $ findUser username) \_ -> failUserError badRequest400 UsernameTaken

  newUuid <- liftIO UUID.nextRandom
  hashedPw <- hashPw password
  lift $ storeUser $ User (UserId newUuid) username hashedPw Nothing
  answerOk ()

authenticate ::
  (Members '[UserService, JwtAccess] r, MonadIO (Sem r)) =>
  (User -> MyAction r ()) ->
  MyAction r ()
authenticate action = do
  auth <- whenNothingM (S.header "Authorization") $ failUserError forbidden403 NotLoggedIn
  fullToken <- whenNothing (T.stripPrefix "Bearer" (toText auth)) $ failUserError forbidden403 NotBearerAuthenticated

  let jwtToken = SessionToken $ J.Jwt $ encodeUtf8 $ T.strip fullToken
  (user, _) <- loadUserFromToken jwtToken
  action user

listUsers :: (Member UserService r, MonadIO (Sem r)) => User -> MyAction r ()
listUsers _ = do
  users <- lift allUsers

  answerOk $ map userId users

-------------------- HELPERS --------------------

loadUserFromToken ::
  forall typ r.
  (Members '[UserService, JwtAccess] r, MonadIO (Sem r), KnownSymbol typ) =>
  JwtToken typ ->
  MyAction r (User, CustomClaim typ)
loadUserFromToken token = lift (extractClaimData token) >>= either handleError handleResult
  where
    handleError :: TokenError -> MyAction r a
    handleError = \case
      t@TokenMalformed {} -> failUserError unprocessableEntity422 $ AuthTokenError t
      other -> failUserError badRequest400 $ AuthTokenError other

    handleResult :: CustomClaim typ -> MyAction r (User, CustomClaim typ)
    handleResult claim = do
      user <- whenNothingM (lift $ loadUser $ UserId $ sub claim) $ failUserError badRequest400 UserNoLongerExists

      ifM
        (checkPairId claim user)
        (pure (user, claim))
        (failUserError badRequest400 SessionLoggedOut)

    checkPairId :: CustomClaim typ -> User -> MyAction r Bool
    checkPairId claim user = case refreshToken user of
      Nothing -> return False
      Just refToken -> do
        extractedClaim <- lift (extractClaimData refToken)

        let handleRefreshError TokenExpired = do
              lift $ storeUser user {refreshToken = Nothing}
              failUserError badRequest400 LoginExpired
            handleRefreshError _ = S.raise "Stored JWT Token turned invalid?"

        refreshClaim <- either handleRefreshError pure extractedClaim

        return $ pid claim == pid refreshClaim

generateNewTokens :: Members '[UserService, JwtAccess] r => User -> MyAction r ()
generateNewTokens user = do
  tokens <- lift $ generateNewTokenPair $ coerce $ userId user
  lift $ storeUser (user {refreshToken = Just $ jtRefresh tokens})
  answerOk tokens

-- TODO
hashPw :: Monad m => Text -> m ByteString
hashPw = pure . encodeUtf8

verifyPassword :: Monad m => User -> Text -> m Bool
verifyPassword user pw = do
  hashedPw <- hashPw pw
  return $ userPassword user == hashedPw
