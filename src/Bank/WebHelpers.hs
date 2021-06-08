module Bank.WebHelpers
  ( module Bank.WebHelpers,
  )
where

import           Bank.Types
import           Data.Aeson
import qualified Data.List.NonEmpty        as N
import           Network.HTTP.Types.Status (Status, badRequest400, ok200,
                                            unprocessableEntity422)
import           Polysemy
import           Relude
import           Validation
import qualified Web.Scotty.Trans          as S

class FromJSON a => Validatable a where
  validate :: a -> Validation (NonEmpty Text) a
  validate = pure

newtype Always a = Always a
  deriving newtype FromJSON
  deriving anyclass Validatable

answerOk :: ToJSON a => a -> MyAction r b
answerOk answer = do
  S.status ok200
  S.json $ okResult answer
  S.finish

failUserError :: Status -> Text -> MyAction r a
failUserError status text = do
  S.status status
  S.json $ userError text
  S.finish

failInternalError :: Status -> Text -> MyAction r a
failInternalError status text = do
  S.status status
  S.json $ internalError text
  S.finish

parseJsonBody :: (Validatable a, MonadIO (Sem r)) => MyAction r a
parseJsonBody = do
  prevalidated :: a <- S.jsonData `S.rescue` malformedHandler

  validate prevalidated & flip validation pure \errors -> do
    failInternalError badRequest400 $ unlines $ N.toList errors

malformedHandler :: (S.ScottyError e) => e -> MyAction r a
malformedHandler err =
  failInternalError unprocessableEntity422 ("Malformed JSON data: " <> toText (S.showError err))
