module Bank.InMemory
  ( module Bank.InMemory,
  )
where



import           Bank.Data        (User (..), UserService (..))
import           Data.UUID        (UUID)
import           Polysemy
import           Relude           hiding (exp)
import qualified Relude.Extra.Map as Map

runUserServiceInMemory :: forall r a. Member (Embed STM) r => TVar (Map UUID User) -> Sem (UserService : r) a -> Sem r a
runUserServiceInMemory userStorage = interpret \case
  -- StoreUser :: User -> UserService m ()
  StoreUser user -> embed $ modifyTVar' userStorage (Map.insert (userId user) user)
  -- LoadUser :: UUID -> UserService m (Maybe User)
  LoadUser userId -> embed $ Map.lookup userId <$> readTVar userStorage
  -- FindUser :: Text -> UserService m (Maybe User)
  FindUser name -> embed $ find ((== name) . userUsername) <$> readTVar userStorage
