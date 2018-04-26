{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Escaped
import qualified Data.Text.IO as T

main :: IO ()
main = do
    forM_ simpleColors $ \(f, b) -> do
        let label = T.pack $ show f <> " on " <> show b

        T.putStrLn $ render $
            "This text is "
            <> FG f
            <> BG b
            <> Plain label
            <> Reset
            <> "."

    forM_ simpleEscapes $ \e -> do
        let label = T.pack $ show e

        T.putStrLn $ render $
            "This text is "
            <> e
            <> Plain label
            <> Reset
            <> "."

    forM_ [0..255] $ \n -> do
        let label = Plain $ T.center 5 ' ' $ T.pack $ show n
            escaped = render $ FG White <> BG (Custom n) <> label <> Reset

        if (n + 1) `mod` 8 == 0
            then T.putStrLn escaped
            else T.putStr escaped

simpleColors :: [(Color, Color)]
simpleColors =
    [ (Blue, Default)
    , (Cyan, Default)
    , (DarkGray, Default)
    , (Green, Default)
    , (LightBlue, Default)
    , (LightCyan, Default)
    , (LightGray, Default)
    , (LightGreen, Default)
    , (LightMagenta, Default)
    , (LightRed, Default)
    , (LightYellow, Default)
    , (Magenta, Default)
    , (Red, Default)
    , (Yellow, Default)
    , (White, Black)
    , (Black, White)
    ]

simpleEscapes :: [Escaped]
simpleEscapes =
    [ Bold
    , Dim
    , Underlined
    , Blink
    , Reverse
    , Hidden
    ]
