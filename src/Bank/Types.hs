module Bank.Types
  ( module Bank.Types,
  )
where

import           Data.Aeson       (ToJSON (..), Value (..))
import           Polysemy
import           Relude
import           Web.Scotty.Trans (ActionT, ScottyT)

type MyScotty r a = ScottyT LText (Sem r) a

type MyAction r a = ActionT LText (Sem r) a

data Result a = Result
  { result     :: ResultType,
    resultData :: a
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

okResult :: a -> Result a
okResult = Result Ok

userError :: a -> Result a
userError = Result UserError

internalError :: a -> Result a
internalError = Result Error

data ResultType = Ok | UserError | Error

instance ToJSON ResultType where
  toJSON Ok        = String "ok"
  toJSON UserError = String "user-error"
  toJSON Error     = String "error"
