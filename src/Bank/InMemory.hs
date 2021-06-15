module Bank.InMemory
  ( runUserServiceInMemory,
    runAccountServiceInMemory
  )
where

import           Bank.Data        (AccountData (..), AccountEvent (..),
                                   AccountEventIn (..), AccountId (..),
                                   AccountLoadResult (..),
                                   AccountProcessResult (..),
                                   AccountService (..), TimedEvent (..),
                                   User (..), UserId (..), UserService (..),
                                   Whose (..))
import           Data.Time.Clock  (getCurrentTime)
import           Data.UUID.V4     (nextRandom)
import           Polysemy
import           Relude           hiding (exp)
import qualified Relude.Extra.Map as Map

runUserServiceInMemory :: forall r a. Member (Embed STM) r => TVar (Map UserId User) -> Sem (UserService : r) a -> Sem r a
runUserServiceInMemory userStorage = interpret \case
  -- StoreUser :: User -> UserService m ()
  StoreUser user -> embed $ modifyTVar' userStorage (Map.insert (userId user) user)
  -- LoadUser :: UUID -> UserService m (Maybe User)
  LoadUser userId -> embed $ Map.lookup userId <$> readTVar userStorage
  -- FindUser :: Text -> UserService m (Maybe User)
  FindUser name -> embed $ find ((== name) . userUsername) <$> readTVar userStorage
  -- AllUsers :: UserService m [User]
  AllUsers -> embed $ Map.elems <$> readTVar userStorage

runAccountServiceInMemory ::
  forall r a.
  Members '[Embed STM, Embed IO] r =>
  TVar (Map AccountId (UserId, TVar AccountData)) ->
  Sem (AccountService : r) a ->
  Sem r a
runAccountServiceInMemory accountStorage = interpret \case
  -- OpenAccount :: User -> Text -> AccountService m AccountId
  OpenAccount User {userId} name -> do
    currTime <- embed @IO getCurrentTime
    accountId <- embed @IO $ AccountId <$> nextRandom

    let accountData =
          AccountData
            { accountId = accountId,
              accountName = name,
              balance = 0,
              history = TimedEvent currTime (Opened name) :| []
            }

    accountTVar <- embed @IO $ newTVarIO accountData

    embed @STM $ modifyTVar' accountStorage $ Map.insert accountId (userId, accountTVar)

    return accountId
  -- CloseAccount :: User -> AccountId -> AccountService m Bool
  CloseAccount User {userId} accountId -> do
    currTime <- embed @IO getCurrentTime
    embed @STM do
      accounts <- readTVar accountStorage

      Map.lookup accountId accounts & \case
        Nothing -> return False
        Just (storedUserId, _) | storedUserId /= userId -> return False
        Just (_, accountTVar) -> onlyUnclosed accountTVar >>= \case
          Nothing -> return False
          Just account -> do
            writeTVar accountTVar $ account { history = one (TimedEvent currTime Closed) <> history account }
            return True
  -- AccountsBy :: User -> AccountService m [AccountId]
  AccountsBy userId -> embed @STM do
    accountTVars <- readTVar accountStorage <&> (map (snd . snd) . filter ((== userId) . fst . snd) . Map.toPairs)

    accountTVars `forM` \tvar -> do
      account <- readTVar tvar
      return (accountId account, accountName account)
  -- LoadAccount :: User -> AccountId -> AccountService m AccountLoadResult
  LoadAccount User {userId} accountId -> embed @STM do
    readTVar accountStorage <&> Map.lookup accountId >>= \case
      Nothing -> pure NoAccountFound
      Just (storedUserId, _) | storedUserId /= userId -> pure NotYourAccount
      Just (_, accountTVar) -> LoadResult <$> readTVar accountTVar
  -- ProcessEvent :: User -> AccountId -> AccountEventIn -> AccountService m AccountProcessResult
  ProcessEvent User {userId} accountId accEvent -> do
    currTime <- embed @IO getCurrentTime
    embed @STM do
      accounts <- readTVar accountStorage

      Map.lookup accountId accounts & \case
        Nothing -> return $ AccountDoesNotExist Yours
        Just (storedUserId, _) | storedUserId /= userId -> return NotYourAccountToModify
        Just (_, accountTVar) -> onlyUnclosed accountTVar >>= \case
          Nothing -> return $ AccountClosed Yours
          Just yourAccount -> case accEvent of
            DepositedIn amount               -> do
              writeTVar accountTVar $
                yourAccount
                  { balance = balance yourAccount + amount
                  , history = one (TimedEvent currTime $ Deposited amount) <> history yourAccount
                  }
              return AccountOk
            WithdrewIn amount                ->
              if balance yourAccount < amount
              then return NotEnoughBalance
              else do
                writeTVar accountTVar $
                  yourAccount
                    { balance = balance yourAccount - amount
                    , history = one (TimedEvent currTime $ Withdrew amount) <> history yourAccount
                    }
                return AccountOk
            TransferToIn otherAccount amount ->
              if balance yourAccount < amount
              then return NotEnoughBalance
              else do
                Map.lookup accountId accounts & \case
                  Nothing -> return $ AccountDoesNotExist Theirs
                  Just (_, theirTVar) -> onlyUnclosed theirTVar >>= \case
                    Nothing -> return $ AccountClosed Theirs
                    Just theirAccount -> do
                      writeTVar accountTVar $
                        yourAccount
                          { balance = balance yourAccount - amount
                          , history = one (TimedEvent currTime $ TransferTo otherAccount amount) <> history yourAccount
                          }
                      writeTVar theirTVar $
                        theirAccount
                          { balance = balance theirAccount + amount
                          , history = one (TimedEvent currTime $ TransferFrom accountId amount) <> history theirAccount
                          }
                      return AccountOk
  where
    onlyUnclosed :: TVar AccountData -> STM (Maybe AccountData)
    onlyUnclosed accTVar = do
      acc <- readTVar accTVar

      if event (head (history acc)) == Closed
        then return Nothing
        else return $ Just acc
