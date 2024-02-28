{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Yaml.MaybeVariable (
  MaybeVariable (..),
  VariableName (..),
  parseVariable,
  toEitherWithVariablesEnv,
  buildWithVariableEnv,
) where

import Barbies (ConstraintsB (..), TraversableB, bmapC, bsequence')
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (parseEither)
import Data.Attoparsec.Text
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.Generics

newtype VariableName = VariableName Text
  deriving stock (Show, Eq, Generic)
  deriving (Hashable) via Text
  deriving (FromJSON) via Text
  deriving (ToJSON) via Text

data MaybeVariable a = Variable VariableName | JsonPrimValue a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (FromJSON a) => FromJSON (MaybeVariable a) where
  parseJSON (String s) =
    case parseVariable s of
      Right v -> pure . Variable $ VariableName v
      Left _ -> JsonPrimValue <$> parseJSON (String s)
  -- FIXME: {}が含まれる文字列をエラーにする
  parseJSON v = JsonPrimValue <$> parseJSON v

parseVariable :: Text -> Either String Text
parseVariable =
  parseOnly $ ("{{" *> takeWhile1 (/= '}') <* "}}") <* endOfInput

toEitherWithVariablesEnv :: forall a. (FromJSON a) => HM.HashMap VariableName Text -> MaybeVariable a -> Either String a
toEitherWithVariablesEnv env = \case
  Variable (VariableName v) ->
    case HM.lookup (VariableName v) env of
      Just t -> eitherDecodeStrict' @a $ T.encodeUtf8 t
      Nothing -> Left $ "Variable " ++ show v ++ " is not found"
  JsonPrimValue a -> Right a

-- | Build a list of records with variable environment
buildWithVariableEnv ::
  forall (b :: (Type -> Type) -> Type).
  ( FromJSON (b MaybeVariable)
  , AllB FromJSON b
  , ConstraintsB b
  , TraversableB b
  ) =>
  -- | The field name of the variable matrix
  Text ->
  -- | The value to be parsed
  Value ->
  -- | The list of records with variable environment
  Either String [b Identity]
buildWithVariableEnv matrixFieldName v = do
  (envs, var) <-
    parseEither
      ( withObject "WithVariables" $ \o -> do
          !envs <- sequenceA . HM.mapKeys VariableName <$> o .: (fromText matrixFieldName)
          !valueWithVariable <- parseJSON @(b MaybeVariable) (Object o)
          pure (envs, valueWithVariable)
      )
      v
  traverse
    (bsequence' . (\env -> bmapC @FromJSON (toEitherWithVariablesEnv env) var))
    envs
