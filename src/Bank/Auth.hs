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

type ElmTypes = '[LoginData]

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

-------------------- ROUTES -------------------

login :: (Members '[UserData, JwtAccess] r, MonadIO (Sem r)) => MyAction r ()
login = do
  LoginData {username, password} <- parseJsonBody

  user <- whenNothingM (lift $ findUser username) $ failUserError forbidden403 "Unknown user"

  ifM
    (not <$> verifyPassword user password)
    (failUserError forbidden403 "Wrong password")
    (generateNewTokens user)

refresh :: (Members '[UserData, JwtAccess] r, MonadIO (Sem r)) => MyAction r ()
refresh = do
  Always SingleToken {token} <- parseJsonBody

  let rToken = RefreshToken $ extractJwtToken token

  -- Loading the user verifies the refresh token.
  (_, refreshClaims) <- loadUserFromToken rToken
  newTokens <- lift $ generateNewSession refreshClaims

  answerOk newTokens

authenticate :: (Members '[UserData, JwtAccess] r, MonadIO (Sem r)) => (User -> MyAction r ()) -> MyAction r ()
authenticate action = do
  auth <- whenNothingM (S.header "Authorization") $ failUserError forbidden403 "Not logged in."
  fullToken <- whenNothing (T.stripPrefix "Bearer" (toText auth)) $ failUserError forbidden403 "Not Bearer authentication."

  let jwtToken = SessionToken $ J.Jwt $ encodeUtf8 $ T.strip fullToken
  (user, _) <- loadUserFromToken jwtToken
  action user

logout :: Member UserData r => User -> MyAction r ()
logout user = do
  let newUser = user {refreshToken = Nothing}
  lift $ storeUser newUser
  answerOk True

createAccount :: (Members '[UserData] r, MonadIO (Sem r)) => MyAction r ()
createAccount = do
  LoginData {username, password} <- parseJsonBody

  user <- lift $ findUser username
  whenJust user \_ -> failUserError badRequest400 "User with that name already exists."

  newUuid <- liftIO UUID.nextRandom
  hashedPw <- hashPw password
  let newUser = User newUuid username hashedPw Nothing
  lift $ storeUser newUser
  answerOk True

-------------------- HELPERS --------------------

loadUserFromToken ::
  forall typ r.
  (Members '[UserData, JwtAccess] r, MonadIO (Sem r), KnownSymbol typ) =>
  JwtToken typ ->
  MyAction r (User, CustomClaim typ)
loadUserFromToken token = lift (extractClaimData token) >>= either handleError handleResult
  where
    handleError :: TokenError -> MyAction r a
    handleError = \case
      TokenExpired -> failUserError badRequest400 "Token expired."
      TokenWrongType -> failUserError badRequest400 "Wrong token type."
      TokenUnsigned -> failUserError badRequest400 "Token unsigned."
      TokenMalformed text -> failUserError unprocessableEntity422 $ "Token malformed: " <> text

    handleResult :: CustomClaim typ -> MyAction r (User, CustomClaim typ)
    handleResult claim = do
      user <- whenNothingM (lift $ loadUser $ sub claim) $ failUserError badRequest400 "User no longer exists."

      ifM
        (checkPairId claim user)
        (pure (user, claim))
        (failUserError badRequest400 "This session was logged out.")

    checkPairId :: CustomClaim typ -> User -> MyAction r Bool
    checkPairId claim user = case refreshToken user of
      Nothing -> return False
      Just refToken -> do
        extractedClaim <- lift (extractClaimData refToken)

        let handleRefreshError TokenExpired = do
              lift $ storeUser user {refreshToken = Nothing}
              failUserError badRequest400 "Your login has expired."
            handleRefreshError _ = S.raise "Stored JWT Token turned invalid?"

        refreshClaim <- either handleRefreshError pure extractedClaim

        return $ pid claim == pid refreshClaim

generateNewTokens :: Members '[UserData, JwtAccess] r => User -> MyAction r ()
generateNewTokens user = do
  tokens <- lift $ generateNewTokenPair $ userId user
  lift $ storeUser (user {refreshToken = Just $ jtRefresh tokens})
  answerOk tokens

-- TODO
hashPw :: Monad m => Text -> m ByteString
hashPw = pure . encodeUtf8

verifyPassword :: Monad m => User -> Text -> m Bool
verifyPassword user pw = do
  hashedPw <- hashPw pw
  return $ userPassword user == hashedPw
