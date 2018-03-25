module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Except (runExcept)
import Babylon.Types (parseExpression')
import Data.Either (Either(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case runExcept $ parseExpression' "(x => x)(42)" of
    Right result -> logShow result
    Left errs    -> logShow errs
