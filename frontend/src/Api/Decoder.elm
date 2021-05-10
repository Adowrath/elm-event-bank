module Api.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Api.ElmStreet exposing (..)
import Api.Types as T


decodeExampleType : Decoder T.ExampleType
decodeExampleType = D.succeed T.ExampleType
    |> required "first" D.int
    |> required "second" D.string
