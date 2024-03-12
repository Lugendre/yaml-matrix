module Data.Yaml.Matrix (
  module Data.Yaml.Matrix.Variable,
  VariableMatrix,
  withVariableMatrix,
) where

import Barbies (TraversableB)
import Barbies.Bare (Bare, BareB, Covered)
import Control.Exception.Safe (Exception, MonadThrow, throwM)
import Data.Aeson (Key)
import Data.Aeson.Key (fromText)
import Data.Aeson.Lens (AsJSON (_JSON), AsValue, key)
import Data.Functor.Identity (Identity)
import Data.Reflection (Given)
import Data.Text (Text)
import Data.Yaml.Matrix.Variable
import Lens.Micro ((^?))

type VariableMatrix = [VariableEnv]

decodeVariableMatrix :: (AsValue bs) => bs -> Key -> Maybe VariableMatrix
decodeVariableMatrix bs field = sequenceA <$> bs ^? key field . _JSON

data VariableMatrixException = NotFoundVariableMatrix
  deriving (Show)
instance Exception VariableMatrixException

withVariableMatrix ::
  forall b bs m.
  (TraversableB (b Covered), BareB b, AsValue bs, MonadThrow m) =>
  bs ->
  Text ->
  ((Given VariableEnv) => bs -> m (b Covered WithVariable)) ->
  m [b Bare Identity]
withVariableMatrix bs fieldName decoder = do
  matrix <-
    maybe (throwM NotFoundVariableMatrix) pure
      . decodeVariableMatrix bs
      $ fromText fieldName
  traverse
    (\ !varEnv -> withVariableEnv varEnv bs decoder)
    matrix
