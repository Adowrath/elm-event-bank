module Bank.Account
  ( module Bank.Account,
  )
where

import           Bank.Data
import           Bank.Types
import           Bank.WebHelpers
import           Data.Aeson                as A
import qualified Data.UUID                 as UUID
import           Elm
import           Network.HTTP.Types.Status (badRequest400, forbidden403,
                                            notFound404)
import           Polysemy
import           Relude                    hiding (exp)
import           Validation                as V
import qualified Web.Scotty.Trans          as S

-------------------- COMMUNICATION OBJECTS -------------------

type ElmTypes = '[AccountOpen]

data AccountOpen = AccountOpen { accountOpen :: Text }
  deriving stock (Generic)
  deriving (FromJSON, Elm) via ElmStreet AccountOpen

instance Validatable AccountOpen where
  validate a@(AccountOpen name) =
    a <$ failureIf (name == "") "Empty account names are not allowed."

-------------------- ROUTES -------------------

open :: (Member AccountService r, MonadIO (Sem r)) => User -> MyAction r ()
open user = do
  AccountOpen name <- parseJsonBody

  accountId <- lift $ openAccount user name

  answerOk accountId

close :: (Member AccountService r, MonadIO (Sem r)) => User -> MyAction r ()
close user = do
  (UUID.fromText <$> S.param @Text "id") >>= \case
    Nothing -> S.next
    Just accountId -> do
      result <- lift $ closeAccount user $ AccountId accountId

      unless result $ S.status badRequest400

      answerOk result

listMine :: (Member AccountService r, MonadIO (Sem r)) => User -> MyAction r ()
listMine User{userId} = do
  accounts <- lift $ accountsBy userId

  answerOk accounts

listBy :: (Member AccountService r, MonadIO (Sem r)) => User -> MyAction r ()
listBy _ = do
  (UUID.fromText <$> S.param @Text "user") >>= \case
    Nothing -> S.next
    Just userId -> do
      result <- lift $ accountsBy $ UserId userId

      answerOk result

get :: (Member AccountService r, MonadIO (Sem r)) => User -> MyAction r ()
get user = do
  (UUID.fromText <$> S.param @Text "id") >>= \case
    Nothing -> S.next
    Just accountId -> do
      result <- lift $ loadAccount user $ AccountId accountId

      case result of
        NoAccountFound -> S.status notFound404
        NotYourAccount -> S.status forbidden403
        LoadResult _   -> pass

      answerOk result

update :: (Member AccountService r, MonadIO (Sem r)) => User -> MyAction r ()
update user = do
  (UUID.fromText <$> S.param @Text "id") >>= \case
    Nothing -> S.next
    Just accountId -> do
      Always event <- parseJsonBody

      result <- lift $ processEvent user (AccountId accountId) event

      case result of
        AccountOk              -> pass
        NotYourAccountToModify -> S.status forbidden403
        NotEnoughBalance       -> S.status badRequest400
        AccountClosed _        -> S.status badRequest400
        AccountDoesNotExist _  -> S.status notFound404

      answerOk result
