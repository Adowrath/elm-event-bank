module Bank.Types
  ( module Bank.Types,
  )
where

import           Data.Aeson       (ToJSON (..), Value (..), object, pairs, (.=))
import           Elm              (Elm (..))
import           Polysemy
import           Relude
import           Web.Scotty.Trans (ActionT, ScottyT)

type MyScotty r a = ScottyT LText (Sem r) a

type MyAction r a = ActionT LText (Sem r) a

data Result a = Result
  { result     :: ResultType,
    resultData :: a
  }

instance (Elm a, ToJSON a) => ToJSON (Result a) where
  toJSON Result {result, resultData} =
    object
      [ "result" .= result,
        "data" .= resultData
      ]
  toEncoding Result {result, resultData} =
    pairs
      ( "result" .= result
          <> "data" .= resultData
      )

okResult :: a -> Result a
okResult = Result Ok

userError :: a -> Result a
userError = Result Error

internalError :: a -> Result a
internalError = Result OtherError

data ResultType = Ok | Error | OtherError

instance ToJSON ResultType where
  toJSON Ok         = String "ok"
  toJSON Error      = String "user-error"
  toJSON OtherError = String "other-error"
