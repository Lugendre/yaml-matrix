{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Yaml.Matrix.VariableSpec (spec) where

import Barbies
import Barbies.Bare
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecodeStrict, encode)
import Data.ByteString (ByteString, toStrict)
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as HM
import Data.Yaml.Matrix.Variable
import GHC.Generics (Generic)
import Test.Hspec

spec :: Spec
spec = do
  describe "simple decode tests with variables" $ do
    it "should be equal" $ do
      withVariableEnv
        mempty
        json1
        (eitherDecodeStrict @(HogeB Covered WithVariable))
        `shouldBe` Right example1
    it "should be equal" $ do
      withVariableEnv
        variableEnv1
        json2
        (eitherDecodeStrict @(HogeB Covered WithVariable))
        `shouldBe` Right example1

data HogeB t f
  = HogeB
  { hoge :: Wear t f Int
  , fuga :: Wear t f String
  }
  deriving stock (Generic)

instance FunctorB (HogeB Bare)
instance FunctorB (HogeB Covered)
instance TraversableB (HogeB Bare)
instance TraversableB (HogeB Covered)
instance DistributiveB (HogeB Covered)
instance ConstraintsB (HogeB Bare)
instance ConstraintsB (HogeB Covered)
instance ApplicativeB (HogeB Covered)
instance BareB HogeB
deriving instance (AllB Show (HogeB Bare)) => Show (HogeB Bare f)
deriving instance (AllB Eq (HogeB Bare)) => Eq (HogeB Bare f)
deriving instance (AllBF Show f (HogeB Covered)) => Show (HogeB Covered f)
deriving instance (AllBF Eq f (HogeB Covered)) => Eq (HogeB Covered f)
deriving instance (AllB ToJSON (HogeB Bare)) => ToJSON (HogeB Bare f)
deriving instance (AllBF ToJSON f (HogeB Covered)) => ToJSON (HogeB Covered f)
deriving instance (AllB FromJSON (HogeB Bare)) => FromJSON (HogeB Bare f)
deriving instance (AllBF FromJSON f (HogeB Covered)) => FromJSON (HogeB Covered f)

example1 :: HogeB Bare Identity
example1 = HogeB{hoge = 1, fuga = "fuga"}

json1 :: ByteString
json1 = toStrict $ encode example1

json2 :: ByteString
json2 = "{\"hoge\": \"${{hoge}}\", \"fuga\": \"${{fu}}${{ga}}\"}"

variableEnv1 :: VariableEnv
variableEnv1 =
  HM.fromList
    [ (VariableName "hoge", Variable (Number 1))
    , (VariableName "fu", Variable (String "fu"))
    , (VariableName "ga", Variable (String "ga"))
    ]