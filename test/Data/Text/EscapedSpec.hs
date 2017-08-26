{-# LANGUAGE OverloadedStrings #-}
module Data.Text.EscapedSpec
    ( main
    , spec
    ) where

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Text.Escaped
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "visibleLength" $ do
        it "calculates the visible length of escaped text" $ do
            let l1 = visibleLength $ "Some text."
                l2 = visibleLength $ FG Red <> "Some text" <> Reset <> "."

            l1 `shouldBe` 10
            l2 `shouldBe` 10

    describe "foreground color helpers" $ do
        forM_
            [ (black, Black)
            , (blue, Blue)
            , (cyan, Cyan)
            , (darkGray, DarkGray)
            , (green, Green)
            , (lightBlue, LightBlue)
            , (lightCyan, LightCyan)
            , (lightGray, LightGray)
            , (lightGreen, LightGreen)
            , (lightMagenta, LightMagenta)
            , (lightRed, LightRed)
            , (lightYellow, LightYellow)
            , (magenta, Magenta)
            , (red, Red)
            , (white, White)
            , (yellow, Yellow)
            ]
            $ \(h, c) -> it ("uses the correct color for " <> show c) $
                h "" `shouldBe` Many [FG c, Plain "", Reset]
