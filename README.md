# escaped

Produce `Text` with terminal escape sequences.

## Installation

*TODO*: Unreleased as of yet.

## Usage

### Quick Start

```hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Data.Text.Escaped
import qualified Data.Text.IO as T

main :: IO ()
main = T.putStrLn $ render $ "This is " <> red "red" <> " text."
```

### Helper Functions

The above uses the `red` helper function, which prefixes the given value with
the (foreground) color red, then resets after.

Note: this function composes `Escaped`s, so you can nest:

```hs
"This is " <> red (bg Blue "red on blue") <> " text."
```

### The `Escaped` Type

The `Escaped` type has constructors for the various shell escapes and is meant
to be used directly for non-trivial cases.


```hs
"This is " <> Blink <> FG Red <> "blinking red" <> Reset <> " text."
```

### Using `OverLoadedStrings`

The literal `"text"` values in the above examples are equivalent to `Plain
"text"` through the `OverloadedStrings` extension. If you need to interpolate
*non-literals* of type `Text`, don't forget the constructor:

```hs
logMessage :: Level -> Text -> Escaped
logMessage l msg = "[" <> prefix <> "] " <> Plain msg
  where
    prefix :: Level -> Escaped
    prefix Info = blue "INFO"
    prefix Warn = yellow "WARN"
    -- etc.
```

### Terminal Applications

The `terminalRenderer` function queries if `stdout` is connected to a tty and
returns `render` or `plain` as appropriate:

```hs
main = do
    r <- terminalRenderer

    T.putStrLn $ r $
        "This will be escaped as "
        <> red "red"
        <> " only if we're connected to a tty."
```

## Development & Testing

```console
stack setup
stack build --pedantic test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
