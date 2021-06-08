{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ExampleSpec (spec) where

import           Relude

import           Data.Aeson                (Value (..), object, (.=))
import           Network.HTTP.Types.Header
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Lib                       (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
  describe "GET /hello" $ do
    it "responds with 200" $ do
      get "/hello" `shouldRespondWith` 200

    it "responds with 'hello'" $ do
      get "/hello" `shouldRespondWith` "hello"

    it "responds with 200 / 'hello'" $ do
      get "/hello" `shouldRespondWith` "hello" {matchStatus = 200}

    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
      get "/hello" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  describe "GET /some-json" $ do
    it "responds with some JSON" $ do
      get "/some-json" `shouldRespondWith` expectedJsonResponse

expectedJsonResponse =
  let ResponseMatcher status headers body = [json|{foo: 23, bar: 42}|]
  in ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body
