{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Yaml.Matrix.Variable (
  VariableName (..),
  Variable (..),
  VariableEnv,
  WithVariable (..),
  withVariableEnv,
  decodeJSON,
  decodeYaml,
) where

import Barbies (TraversableB, bsequence')
import Barbies.Bare (Bare, BareB (bstrip), Covered)
import Control.Applicative ((<|>))
import Control.Exception.Safe (MonadThrow)
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON, ToJSONKey, Value (..), throwDecodeStrict)
import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as A
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Functor (void)
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Reflection (Given, give, given)
import Data.Text (Text)
import Data.Yaml qualified as Y
import GHC.Generics (Generic, Generic1, Generically1 (..))

newtype VariableName = VariableName Text
  deriving stock (Show, Eq, Generic)
  deriving (Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Text

newtype Variable = Variable Value
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Value

type VariableEnv = HM.HashMap VariableName Variable

newtype WithVariable a = WithVariable {runWithVariable :: a}
  deriving stock (Show, Eq, Generic, Generic1, Functor, Foldable, Traversable)
  deriving (ToJSON) via a
  deriving (Applicative) via Generically1 WithVariable

instance (Given VariableEnv, FromJSON a) => FromJSON (WithVariable a) where
  parseJSON (String s) =
    case parseVariable (given @VariableEnv) s of
      Right v -> WithVariable <$> parseJSON @a v
      Left _e -> WithVariable <$> parseJSON @a (String s)
  parseJSON v = WithVariable <$> parseJSON @a v

parseVariable :: VariableEnv -> Text -> Either String Value
parseVariable varEnv =
  parseOnly
    ( (variableWithText varEnv <|> variableOnly varEnv) <* endOfInput
    )

variableWithText :: VariableEnv -> Parser Value
variableWithText varEnv = do
  let withPre = do
        pre <- A.takeWhile (/= '$')
        name <- variable
        var <- maybe (fail "") pure $ HM.lookup name varEnv -- TODO: エラーメッセージ
        case var of
          Variable (String s) -> pure (pre <> s)
          Variable _v -> fail "" -- TODO: エラーメッセージ
  values <- fold <$> many1' withPre
  post <- takeText
  pure . String $ values <> post

variableOnly :: VariableEnv -> Parser Value
variableOnly varEnv = do
  name <- variable
  maybe (fail "") (pure . coerce) $ HM.lookup name varEnv

variable :: Parser VariableName
variable = do
  void "${{"
  name <- takeWhile1 (/= '}')
  void "}}"
  pure $ VariableName name

withVariableEnv ::
  VariableEnv ->
  ((Given VariableEnv) => bs -> m (b Bare Identity)) ->
  bs ->
  m (b Bare Identity)
withVariableEnv = give

decodeJSON ::
  forall b m.
  ( TraversableB (b Covered)
  , BareB b
  , MonadThrow m
  , FromJSON (b Covered WithVariable)
  ) =>
  ByteString ->
  m (b Bare Identity)
decodeJSON bs =
  bstrip . runWithVariable . bsequence'
    <$> throwDecodeStrict @(b Covered WithVariable) @m bs

decodeYaml ::
  forall (b :: Type -> (Type -> Type) -> Type) m.
  ( MonadThrow m
  , FromJSON (b Covered WithVariable)
  , TraversableB (b Covered)
  , BareB b
  ) =>
  ByteString ->
  m (b Bare Identity)
decodeYaml bs =
  bstrip . runWithVariable . bsequence'
    <$> Y.decodeThrow @m @(b Covered WithVariable) bs