{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Bank.Jwt
  ( -- * Tokens
    JwtTokensPair (..),
    JwtToken,
    extractJwtToken,
    SessionToken (..),
    RefreshToken (..),
    SingleToken (..),

    -- * JWT Effect
    JwtAccess (..),
    generateNewTokenPair,
    generateNewSession,
    extractClaimData,
    runRealJwt,
    CustomClaim (..),
    TokenError (..),

    -- * Elm types
    ElmTypes,
  )
where

import           Data.Aeson            as A (FromJSON (..), ToJSON,
                                             eitherDecode', encode, withObject,
                                             (.:))
import qualified Data.Text             as T (isPrefixOf)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.UUID             (UUID)
import qualified Data.UUID.V4          as UUID (nextRandom)
import           Elm                   (Elm (..), ElmAlias (..),
                                        ElmDefinition (..), ElmPrim (..),
                                        ElmRecordField (..), ElmStreet (..),
                                        TypeRef (..))
import           GHC.TypeLits          (KnownSymbol, symbolVal)
import qualified Jose.Jwk              as J
import qualified Jose.Jwt              as J
import           Polysemy
import           Relude                hiding (exp)
import           Unsafe.Coerce         (unsafeCoerce)

-------------------- COMMUNICATION OBJECTS -------------------

type ElmTypes = '[SessionToken, RefreshToken, JwtTokensPair, SingleToken ""]

type family JwtToken a = r | r -> a where
  JwtToken "session" = SessionToken
  JwtToken "refresh" = RefreshToken

extractJwtToken :: JwtToken a -> J.Jwt
extractJwtToken = unsafeCoerce

newtype SessionToken = SessionToken J.Jwt deriving newtype (FromJSON, ToJSON)

newtype RefreshToken = RefreshToken J.Jwt deriving newtype (FromJSON, ToJSON)

instance Elm SessionToken where toElmDefinition _ = DefPrim ElmString

instance Elm RefreshToken where toElmDefinition _ = DefPrim ElmString

data JwtTokensPair = JwtTokensPair
  { jtSession :: SessionToken,
    jtRefresh :: RefreshToken
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON, Elm) via ElmStreet JwtTokensPair

newtype SingleToken typ = SingleToken {token :: JwtToken typ}

instance FromJSON (SingleToken typ) where
  -- We use SessionToken as pseudo-result so the parsing works even with the quantified 'typ' type variable.
  -- We can't require all type instances of JwtToken to be Coercible to J.Jwt, but the unsafeCoerce is safe.
  parseJSON = withObject "SingleToken" $ \v ->
    unsafeCoerce $
      SingleToken
        <$> (coerce @J.Jwt @SessionToken <$> v .: "token")

instance Elm (SingleToken typ) where
  toElmDefinition _ = DefAlias $ ElmAlias "SingleToken" (ElmRecordField (RefPrim ElmString) "token" :| []) False

-------------------- EFFECTS --------------------

data TokenError = TokenMalformed Text | TokenUnsigned | TokenWrongType | TokenExpired

data CustomClaim a = CustomClaim
  { -- | Issuer
    iss :: !Text,
    -- | Subject
    sub :: !UUID,
    -- | Expiration Time
    exp :: !J.IntDate,
    -- | Issued at
    iat :: !J.IntDate,
    -- | Type - "refresh" or "session"
    typ :: !Text,
    -- | Pair ID - Used for invalidation
    pid :: !UUID
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data JwtAccess m a where
  GenerateNewTokenPair :: UUID -> JwtAccess m JwtTokensPair
  GenerateNewSession :: CustomClaim "refresh" -> JwtAccess m (JwtToken "session")
  ExtractClaimData :: forall typ m. KnownSymbol typ => JwtToken typ -> JwtAccess m (Either TokenError (CustomClaim typ))

makeSem ''JwtAccess

runRealJwt :: forall r a. MonadIO (Sem r) => [J.Jwk] -> J.JwtEncoding -> Sem (JwtAccess : r) a -> Sem r a
runRealJwt keys encoding = interpret \case
  GenerateNewTokenPair uuid -> do
    curTime <- liftIO getPOSIXTime
    pairUuid <- liftIO UUID.nextRandom

    let baseClaim =
          CustomClaim
            { iss = "event-bank",
              sub = uuid,
              iat = J.IntDate curTime,
              exp = J.IntDate curTime, -- Only temporary.
              typ = "",
              pid = pairUuid
            }
        -- Session Token is valid for 15 minutes.
        sessionClaim = baseClaim {exp = J.IntDate $ curTime + 15 * 60, typ = "session"}
        -- Refresh Token is valid for a month.
        refreshClaim = baseClaim {exp = J.IntDate $ curTime + 30 * 24 * 60 * 60, typ = "refresh"}
        sessionPayload = J.Claims $ fromLazy $ A.encode sessionClaim
        refreshPayload = J.Claims $ fromLazy $ A.encode refreshClaim

    sessionEncode <- liftIO $ SessionToken <<$>> J.encode keys encoding sessionPayload
    refreshEncode <- liftIO $ RefreshToken <<$>> J.encode keys encoding refreshPayload

    return $ either encodeErrorHandler id (JwtTokensPair <$> sessionEncode <*> refreshEncode)
  GenerateNewSession refreshClaim -> do
    curTime <- liftIO getPOSIXTime

    let newSessionClaim =
          CustomClaim
            { iss = "event-bank",
              sub = sub refreshClaim,
              iat = J.IntDate curTime,
              exp = J.IntDate $ curTime + 15 * 60,
              typ = "refresh",
              pid = pid refreshClaim
            }
        newSessionPayload = J.Claims $ fromLazy $ A.encode newSessionClaim

    sessionEncode <- liftIO $ SessionToken <<$>> J.encode keys encoding newSessionPayload

    return $ either encodeErrorHandler id sessionEncode
  ExtractClaimData (jwtToken :: JwtToken typ) -> do
    let (J.Jwt token) = extractJwtToken jwtToken

    curTime <- liftIO getPOSIXTime
    decodeResult <- liftIO $ J.decode keys (Just encoding) token

    return $ do
      jwtContent <- first decodeErrorHandler decodeResult
      rawClaim <- case jwtContent of
        J.Jws (_, content) -> return content
        _                  -> Left $ TokenMalformed "Not a valid JWS Token."

      claim <- first (TokenMalformed . toText) $ A.eitherDecode' $ toLazy rawClaim

      if
          | typ claim /= toText (symbolVal (Proxy :: Proxy typ)) -> Left TokenWrongType
          | coerce (exp claim) < curTime -> Left TokenExpired
          | otherwise -> return claim
  where
    encodeErrorHandler :: J.JwtError -> any
    encodeErrorHandler = \case
      J.KeyError {} -> error "ERROR CASE - Non-matching key and algo."
      J.BadAlgorithm {} -> error "ERROR CASE - Unsupported Algorithm."
      J.BadCrypto -> error "ERROR CASE - Malformed key?"
      _ -> error "Can't happen here."

    decodeErrorHandler :: J.JwtError -> TokenError
    decodeErrorHandler = \case
      J.BadAlgorithm text
        | "JWT is unsecured" `T.isPrefixOf` text -> TokenUnsigned
        | otherwise -> TokenMalformed text
      J.KeyError text -> TokenMalformed text
      J.BadHeader text -> TokenMalformed text
      J.BadCrypto -> TokenMalformed "Invalid token."
      J.Base64Error text -> TokenMalformed $ toText text
      J.BadDots {} -> error "Shouldn't happen - we don't call JWT.decodeClaims here."
      J.BadClaims -> error "Can't happen here."
      J.BadSignature -> TokenUnsigned
