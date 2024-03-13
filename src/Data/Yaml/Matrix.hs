module Data.Yaml.Matrix (
  module Data.Yaml.Matrix.Variable,
  VariableMatrix,
  withVariableMatrix,
  withVariableMatrix',
  decodeVariableMatrixJSON,
  decodeVariableMatrixYaml,
  VariableMatrixException (..),
) where

import Barbies.Bare (Bare)
import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.Key (fromText)
import Data.Aeson.Lens (AsJSON (_JSON), key)
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict qualified as HM
import Data.Reflection (Given)
import Data.Text (Text)
import Data.Yaml qualified as Y
import Data.Yaml.Matrix.Variable
import Lens.Micro ((^?))

type VariableMatrix = [VariableEnv]

-- | only for test
decodeVariableMatrixJSON :: ByteString -> Text -> Maybe VariableMatrix
decodeVariableMatrixJSON bs field = sequenceA <$> bs ^? key (fromText field) . _JSON

decodeVariableMatrixYaml :: (MonadThrow m) => ByteString -> Text -> m VariableMatrix
decodeVariableMatrixYaml bs fieldName = do
  section <- Y.decodeThrow @_ @(HM.HashMap Text Y.Value) bs
  matrices <- maybe (throwM NotFoundVariableMatrix) pure $ HM.lookup fieldName section
  case fromJSON matrices of
    Error e -> throwM $ FailedDecodeVariableMatrix e
    Success v -> pure $ sequenceA v

data VariableMatrixException
  = NotFoundVariableMatrix
  | FailedDecodeVariableMatrix String
  deriving (Show)
instance Exception VariableMatrixException

-- | only for test
withVariableMatrix' ::
  forall b m bs.
  ( MonadThrow m
  ) =>
  VariableMatrix ->
  ((Given VariableEnv) => bs -> m (b Bare Identity)) ->
  bs ->
  m [b Bare Identity]
withVariableMatrix' matrix f bs =
  traverse
    (\ !varEnv -> withVariableEnv varEnv f bs)
    matrix

withVariableMatrix ::
  forall b m.
  ( MonadThrow m
  ) =>
  Text ->
  ((Given VariableEnv) => ByteString -> m (b Bare Identity)) ->
  ByteString ->
  m [b Bare Identity]
withVariableMatrix fieldName f bs = do
  matrix <- decodeVariableMatrixYaml bs fieldName
  withVariableMatrix' matrix f bs
