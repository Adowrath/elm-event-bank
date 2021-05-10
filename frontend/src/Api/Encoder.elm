module Api.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Api.ElmStreet exposing (..)
import Api.Types as T


encodeExampleType : T.ExampleType -> Value
encodeExampleType x = E.object
    [ ("tag", E.string "ExampleType")
    , ("first", E.int x.first)
    , ("second", E.string x.second)
    ]
