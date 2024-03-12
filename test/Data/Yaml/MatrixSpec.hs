{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Yaml.MatrixSpec (spec) where

import Barbies
import Barbies.Bare
import Data.Aeson (FromJSON, ToJSON, encode, throwDecodeStrict)
import Data.ByteString (ByteString, toStrict)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Yaml.Matrix
import GHC.Generics (Generic)
import Test.Hspec

spec :: Spec
spec = describe "simple decode tests with variables matrix" $ do
  it "should be equal" $ do
    withVariableMatrix @HogeB
      json1
      "matrix"
      throwDecodeStrict
      `shouldBe` Just [example1]

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

data MatrixJSON = MatrixJSON
  { hoge :: [Int]
  , fuga :: [Text]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)

data HogeJSON = HogeJSON
  { matrix :: MatrixJSON
  , hoge :: Text
  , fuga :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)

example1 :: HogeB Bare Identity
example1 = HogeB{hoge = 1, fuga = "fuga"}

json1 :: ByteString
json1 =
  let
    matrix = MatrixJSON{hoge = [1], fuga = ["fuga"]}
    hoge = "${{hoge}}"
    fuga = "${{fuga}}"
   in
    toStrict $ encode HogeJSON{..}