module Data.Text.Escaped
  ( Escaped
      ( Plain
      , Reset
      , Bold
      , Dim
      , Underlined
      , Blink
      , Reverse
      , Hidden
      , FG
      , BG
      )
  , Color (..)
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

import Prelude

import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text qualified as T
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

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
  deriving stock (Eq, Show)

instance Arbitrary Color where
  -- Too bad @Custom@ makes arbitraryBoundedEnum not useful
  arbitrary =
    oneof
      [ pure Default
      , Custom <$> arbitrary
      , pure Black
      , pure Blue
      , pure Cyan
      , pure DarkGray
      , pure Green
      , pure LightBlue
      , pure LightCyan
      , pure LightGray
      , pure LightGreen
      , pure LightMagenta
      , pure LightRed
      , pure LightYellow
      , pure Magenta
      , pure Red
      , pure White
      , pure Yellow
      ]

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
  deriving stock (Eq, Show)

instance Arbitrary Escaped where
  -- Avoid invalid Many-of-Many nesting, which triggers an infinite loop. Such
  -- a value is impossible to construct because we don't export the Many
  -- constructor.
  arbitrary = oneof $ chunks <> [Many <$> listOf1 (oneof chunks)]
   where
    chunks =
      [ Plain <$> arbitrary
      , pure Reset
      , pure Bold
      , pure Dim
      , pure Underlined
      , pure Blink
      , pure Reverse
      , pure Hidden
      , FG <$> arbitrary
      , BG <$> arbitrary
      ]

instance IsString Escaped where
  fromString = Plain . pack

instance Semigroup Escaped where
  -- A bit explicit, but ensures identity works
  x <> (Plain t) | T.null t = x
  (Plain t) <> y | T.null t = y
  (Many a) <> (Many b) = Many $ a <> b
  (Many a) <> b = Many $ a <> [b]
  a <> (Many b) = Many $ a : b
  a <> b = Many [a, b]

instance Monoid Escaped where
  mempty = Plain ""
  mappend = (<>)

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
render :: Escaped -> Text
render = \case
  Plain t -> t
  Many es -> mconcat $ map render es
  Reset -> "\ESC[0m"
  Bold -> "\ESC[1m"
  Dim -> "\ESC[2m"
  Underlined -> "\ESC[3m"
  Blink -> "\ESC[5m"
  Reverse -> "\ESC[7m"
  Hidden -> "\ESC[8m"
  FG c -> "\ESC[" <> fgColorCode c <> "m"
  BG c -> "\ESC[" <> bgColorCode c <> "m"

-- | Render only the @'Text'@ parts
--
-- Examples
--
-- >>> plain $ Plain "Some text."
-- "Some text."
--
-- >>> plain $ "Some " <> FG Red <> "red" <> Reset <> " text."
-- "Some red text."
plain :: Escaped -> Text
plain = \case
  Plain t -> t
  Many es -> mconcat $ map plain es
  _ -> ""

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
fg :: Color -> Escaped -> Escaped
fg = esc . FG

-- | Escape with background @'Color'@, then @'Reset'@
--
-- >>> bg Red "red"
-- Many [BG Red,Plain "red",Reset]
bg :: Color -> Escaped -> Escaped
bg = esc . BG

-- | Apply the given escape, then @'Reset'@
--
-- >>> esc (FG Red) "red"
-- Many [FG Red,Plain "red",Reset]
esc :: Escaped -> Escaped -> Escaped
esc a b = a <> b <> Reset

-- | Calculate the /visible/ length of an @'Escaped'@
visibleLength :: Escaped -> Int
visibleLength = \case
  Plain t -> T.length t
  Many es -> sum $ map visibleLength es
  _ -> 0

-- | An @'IO'@ action to produce the appropriate renderer for a terminal
--
-- Returns @'render'@ if @stdout@ is a terminal, otherwise @'plain'@
--
-- >>> r <- terminalRenderer
-- >>> print $ r $ red "red text"
-- "red text"
terminalRenderer :: IO (Escaped -> Text)
terminalRenderer = do
  istty <- queryTerminal stdOutput
  return $ if istty then render else plain

fgColorCode :: Color -> Text
fgColorCode = \case
  Default -> "39"
  Custom n -> "38;5;" <> pack (show n)
  Black -> "30"
  Blue -> "34"
  Cyan -> "36"
  DarkGray -> "90"
  Green -> "32"
  LightBlue -> "94"
  LightCyan -> "96"
  LightGray -> "37"
  LightGreen -> "92"
  LightMagenta -> "95"
  LightRed -> "91"
  LightYellow -> "93"
  Magenta -> "35"
  Red -> "31"
  White -> "97"
  Yellow -> "33"

bgColorCode :: Color -> Text
bgColorCode = \case
  Default -> "49"
  Custom n -> "48;5;" <> pack (show n)
  Black -> "40"
  Blue -> "44"
  Cyan -> "46"
  DarkGray -> "100"
  Green -> "42"
  LightBlue -> "104"
  LightCyan -> "106"
  LightGray -> "100"
  LightGreen -> "102"
  LightMagenta -> "105"
  LightRed -> "101"
  LightYellow -> "103"
  Magenta -> "45"
  Red -> "41"
  White -> "107"
  Yellow -> "103"
