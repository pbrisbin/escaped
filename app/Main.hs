module Main (main) where

import Prelude

import Data.Foldable (for_)
import Data.Text (pack)
import Data.Text qualified as T
import Data.Text.Escaped
import Data.Text.IO qualified as T

main :: IO ()
main = do
  for_ simpleColors $ \(f, b) -> do
    let label = pack $ show f <> " on " <> show b

    T.putStrLn $
      render $
        "This text is "
          <> FG f
          <> BG b
          <> Plain label
          <> Reset
          <> "."

  for_ simpleEscapes $ \e -> do
    let label = pack $ show e

    T.putStrLn $
      render $
        "This text is "
          <> e
          <> Plain label
          <> Reset
          <> "."

  for_ [0 .. 255] $ \n -> do
    let
      label = Plain $ T.center 5 ' ' $ pack $ show n
      escaped = render $ FG White <> BG (Custom n) <> label <> Reset

    if (n + 1) `mod` 8 == 0 then T.putStrLn escaped else T.putStr escaped

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
simpleEscapes = [Bold, Dim, Underlined, Blink, Reverse, Hidden]
