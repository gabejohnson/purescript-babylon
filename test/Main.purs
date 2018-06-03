module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Control.Monad.Except (runExcept)
import Babylon.Types (parseExpression')
import Data.Either (Either(..))

main :: Effect Unit
main = do
  case runExcept $ parseExpression' "(x => x)(42)" of
    Right result -> logShow result
    Left errs    -> logShow errs
