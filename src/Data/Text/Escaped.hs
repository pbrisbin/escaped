{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Escaped
    ( Color(..)
    , Escaped(..)
    , black
    , blue
    , cyan
    , darkGray
    , green
    , lightBlue
    , lightCyan
    , lightGray
    , lightGreen
    , lightMagenta
    , lightRed
    , lightYellow
    , magenta
    , red
    , white
    , yellow
    , fg
    , bg
    , esc
    , render
    , plain
    , visibleLength
    , terminalRenderer
    ) where

import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)

import qualified Data.Text as T

-- | Supported colors
data Color
    = Default
    | Custom Int
    | Black
    | Blue
    | Cyan
    | DarkGray
    | Green
    | LightBlue
    | LightCyan
    | LightGray
    | LightGreen
    | LightMagenta
    | LightRed
    | LightYellow
    | Magenta
    | Red
    | White
    | Yellow
    deriving (Eq, Show)

-- | Bits of escaped text
data Escaped
    = Plain Text
    | Reset
    | Bold
    | Dim
    | Underlined
    | Blink
    | Reverse
    | Hidden
    | FG Color
    | BG Color
    | Many [Escaped]
    deriving (Eq, Show)

instance IsString Escaped where
    fromString = Plain . T.pack

instance Monoid Escaped where
    mempty = Plain ""
    mappend (Many a) (Many b) = Many $ a ++ b
    mappend (Many a) b = Many $ a ++ [b]
    mappend a (Many b) = Many $ a:b
    mappend a b = Many [a, b]

-- | Render an @'Escaped'@ to actually-escaped @'Text'@
--
-- Examples:
--
-- >>> render "Some text via OverloadedStrings."
-- "Some text via OverloadedStrings."
--
-- >>> render $ Plain "Some text."
-- "Some text."
--
-- >>> render $ "Some " <> FG Red <> "red" <> Reset <> " text."
-- "Some \ESC[31mred\ESC[0m text."
--
-- >>> render $ "Some " <> blue "blue" <> " text."
-- "Some \ESC[34mblue\ESC[0m text."
--
-- >>> render $ "Some " <> fg (Custom 212) "color 212" <> " text."
-- "Some \ESC[38;5;212mcolor 212\ESC[0m text."
--
render :: Escaped -> Text
render (Plain t) = t
render (Many es) = T.concat $ map render es
render Reset = "\ESC[0m"
render Bold = "\ESC[1m"
render Dim = "\ESC[2m"
render Underlined = "\ESC[3m"
render Blink = "\ESC[5m"
render Reverse = "\ESC[7m"
render Hidden = "\ESC[8m"
render (FG c) = "\ESC[" <> fgColorCode c <> "m"
render (BG c) = "\ESC[" <> bgColorCode c <> "m"

-- | Render only the @'Text'@ parts
--
-- Examples
--
-- >>> plain $ Plain "Some text."
-- "Some text."
--
-- >>> plain $ "Some " <> FG Red <> "red" <> Reset <> " text."
-- "Some red text."
--
plain :: Escaped -> Text
plain (Plain t) = t
plain (Many es) = T.concat $ map plain es
plain _ = ""

black :: Escaped -> Escaped
black = fg Black

blue :: Escaped -> Escaped
blue = fg Blue

cyan :: Escaped -> Escaped
cyan = fg Cyan

darkGray :: Escaped -> Escaped
darkGray = fg DarkGray

green :: Escaped -> Escaped
green = fg Green

lightBlue :: Escaped -> Escaped
lightBlue = fg LightBlue

lightCyan :: Escaped -> Escaped
lightCyan = fg LightCyan

lightGray :: Escaped -> Escaped
lightGray = fg LightGray

lightGreen :: Escaped -> Escaped
lightGreen = fg LightGreen

lightMagenta :: Escaped -> Escaped
lightMagenta = fg LightMagenta

lightRed :: Escaped -> Escaped
lightRed = fg LightRed

lightYellow :: Escaped -> Escaped
lightYellow = fg LightYellow

magenta :: Escaped -> Escaped
magenta = fg Magenta

red :: Escaped -> Escaped
red = fg Red

white :: Escaped -> Escaped
white = fg White

yellow :: Escaped -> Escaped
yellow = fg Yellow

-- | Escape with foreground @'Color'@, then @'Reset'@
--
-- >>> fg Red "red"
-- Many [FG Red,Plain "red",Reset]
--
fg :: Color -> Escaped -> Escaped
fg = esc . FG

-- | Escape with background @'Color'@, then @'Reset'@
--
-- >>> bg Red "red"
-- Many [BG Red,Plain "red",Reset]
--
bg :: Color -> Escaped -> Escaped
bg = esc . BG

-- | Apply the given escape, then @'Reset'@
--
-- >>> esc (FG Red) "red"
-- Many [FG Red,Plain "red",Reset]
--
esc :: Escaped -> Escaped -> Escaped
esc a b = a <> b <> Reset

-- | Calculate the /visible/ length of an @'Escaped'@
visibleLength :: Escaped -> Int
visibleLength (Plain t) = T.length t
visibleLength (Many es) = sum $ map visibleLength es
visibleLength _ = 0

-- | An @'IO'@ action to produce the appropriate renderer for a terminal
--
-- Returns @'render'@ if @stdout@ is a terminal, otherwise @'plain'@
--
-- >>> r <- terminalRenderer
-- >>> print $ r $ red "red text"
-- "red text"
--
terminalRenderer :: IO (Escaped -> Text)
terminalRenderer = do
    istty <- queryTerminal stdOutput
    return $ if istty then render else plain

fgColorCode :: Color -> Text
fgColorCode Default = "39"
fgColorCode (Custom n) = "38;5;" <> T.pack (show n)
fgColorCode Black = "30"
fgColorCode Blue = "34"
fgColorCode Cyan = "36"
fgColorCode DarkGray = "90"
fgColorCode Green = "32"
fgColorCode LightBlue = "94"
fgColorCode LightCyan = "96"
fgColorCode LightGray = "37"
fgColorCode LightGreen = "92"
fgColorCode LightMagenta = "95"
fgColorCode LightRed = "91"
fgColorCode LightYellow = "93"
fgColorCode Magenta = "35"
fgColorCode Red = "31"
fgColorCode White = "97"
fgColorCode Yellow = "33"

bgColorCode :: Color -> Text
bgColorCode Default = "49"
bgColorCode (Custom n) = "48;5;" <> T.pack (show n)
bgColorCode Black = "40"
bgColorCode Blue = "44"
bgColorCode Cyan = "46"
bgColorCode DarkGray = "100"
bgColorCode Green = "42"
bgColorCode LightBlue = "104"
bgColorCode LightCyan = "106"
bgColorCode LightGray = "100"
bgColorCode LightGreen = "102"
bgColorCode LightMagenta = "105"
bgColorCode LightRed = "101"
bgColorCode LightYellow = "103"
bgColorCode Magenta = "45"
bgColorCode Red = "41"
bgColorCode White = "107"
bgColorCode Yellow = "103"
