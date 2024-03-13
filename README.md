# yaml-matrix

Yaml generator like the matrix strategy on GitHub Actions

## Examples

You must define Higher Kinded Datatypes in Barbies.

```haskell
module Main (main) where

import Data.Yaml.Matrix
...

data ExampleB t f =
  ExampleB
  {
    hoge :: Wear t f Int
    fuga :: Wear t f Int
  }
  deriving Generic

instance FunctorB (ExampleB Bare)
instance FunctorB (ExampleB Covered)
...
instance BareB ExampleB
deriving instance (AllB FromJSON (ExampleB Bare)) => FromJSON (ExampleB Bare f)
deriving instance (AllBF FromJSON f (ExampleB Covered)) => FromJSON (ExampleB Covered f)

main :: IO ()
main =
  examples <- withFile "example.yaml" $ \bs ->
      withVariableMatrix
        "matrix"
        (decodeYaml @ExampleB)
        bs
  print exmaples
```