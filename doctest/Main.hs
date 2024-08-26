module Main (main) where

import Prelude

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-XDerivingStrategies"
    , "-XImportQualifiedPost"
    , "-XLambdaCase"
    , "-XOverloadedStrings"
    , "src"
    ]
