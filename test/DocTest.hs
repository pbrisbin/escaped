{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.DocTest
import System.FilePath.Glob

main :: IO ()
main = do
    let options =
            [ "-XOverloadedStrings"
            ]

    paths <- globDir1 "**/*.hs" "src"

    doctest $ options ++ paths
