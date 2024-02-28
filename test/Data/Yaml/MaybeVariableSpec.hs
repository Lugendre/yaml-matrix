{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Yaml.MaybeVariableSpec (spec) where

import Data.Aeson (FromJSON (..), Value (Bool, String), decodeStrict, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Yaml.MaybeVariable
import GHC.Generics (Generic)
import Test.Hspec

data TestJSONData f = TestJSONData
  { textField :: f Text
  , doubleField :: f Double
  , arrayField :: f [Text]
  , boolField :: f Bool
  , variableField :: f Text
  }

instance FromJSON (TestJSONData MaybeVariable) where
  parseJSON = withObject "TestJSONData" $ \o ->
    TestJSONData
      <$> o .: "text"
      <*> o .: "double"
      <*> o .: "array"
      <*> o .: "bool"
      <*> o .: "variable"

instance Show (TestJSONData MaybeVariable) where
  show (TestJSONData t d a b v) =
    "TestJSONData { textField = "
      ++ show t
      ++ ", doubleField = "
      ++ show d
      ++ ", arrayField = "
      ++ show a
      ++ ", boolField = "
      ++ show b
      ++ ", variableField = "
      ++ show v
      ++ " }"

instance Eq (TestJSONData MaybeVariable) where
  (==) (TestJSONData t1 d1 a1 b1 v1) (TestJSONData t2 d2 a2 b2 v2) =
    t1 == t2 && d1 == d2 && a1 == a2 && b1 == b2 && v1 == v2

testJSONText :: Text
testJSONText = "{\"text\": \"text\", \"double\": 1.0, \"array\": [\"a\", \"b\"], \"bool\": true, \"variable\": \"{{variable}}\"}"

newtype TestWithVariable = TestWithVariable {hoge :: Double}
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON)

testWithVariablesJSON :: Text
testWithVariablesJSON = "{\"hoge\": 1.0, \"fuga\": \"{{fuga}}\", \"variables\": {\"fuga\": [2.0]}}"

spec :: Spec
spec = describe "plus2" $ do
  it "parse {{variable}}" $
    parseMaybe (parseJSON @(MaybeVariable Text)) (String "{{variable}}")
      `shouldBe` Just (Variable $ VariableName "variable")
  it "parse value" $
    parseMaybe (parseJSON @(MaybeVariable Text)) (String "value")
      `shouldBe` Just (JsonPrimValue "value")
  it "parse {{variable}} value" $
    parseMaybe (parseJSON @(MaybeVariable Text)) (String "{{variable}} value")
      `shouldBe` Just (JsonPrimValue "{{variable}} value")
  it "parse value {{variable}}" $
    parseMaybe (parseJSON @(MaybeVariable Text)) (String "value {{variable}}")
      `shouldBe` Just (JsonPrimValue "value {{variable}}")
  it "parse value {{variable}} value" $
    parseMaybe (parseJSON @(MaybeVariable Text)) (String "value {{variable}} value")
      `shouldBe` Just (JsonPrimValue "value {{variable}} value")
  it "parse {{variable}} value {{variable}}" $
    parseMaybe (parseJSON @(MaybeVariable Text)) (String "{{variable}} value {{variable}}")
      `shouldBe` Just (JsonPrimValue "{{variable}} value {{variable}}")
  it "parse value {{variable}} value {{variable}}" $
    parseMaybe (parseJSON @(MaybeVariable Text)) (String "value {{variable}} value {{variable}}")
      `shouldBe` Just (JsonPrimValue "value {{variable}} value {{variable}}")
  it "parse Bool" $
    parseMaybe (parseJSON @(MaybeVariable Bool)) (Bool True)
      `shouldBe` Just (JsonPrimValue True)
  it "parse TestJSONData" $
    (decodeStrict @(TestJSONData MaybeVariable)) (T.encodeUtf8 testJSONText)
      `shouldBe` Just
        ( TestJSONData
            { textField = JsonPrimValue "text"
            , doubleField = JsonPrimValue 1.0
            , arrayField = JsonPrimValue ["a", "b"]
            , boolField = JsonPrimValue True
            , variableField = Variable $ VariableName "variable"
            }
        )

  it "parse with variables environment" $
    pendingWith "parse with variables environment"