-- From http://stackoverflow.com/questions/5684049/is-there-some-way-to-define-an-enum-in-haskell-that-wraps-around
import Test.HUnit

data CDir = North | East | South | West deriving (Enum, Bounded, Show, Eq)
data CDir2 = N | NE | E | SE | S | SW | W | NW deriving (Enum, Bounded, Show, Eq)

next :: (Enum a, Bounded a) => a -> a
next = turn 1

prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

-- TODO: this is only correct for even-element enumerations
opposite :: (Enum a, Bounded a) => a -> a
opposite e = turn halfNum e
  where halfNum = 1 + (fromEnum (maxBound `asTypeOf` e)) `div` 2

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum $ add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n
    where
      add mod x y = (x + y + mod) `rem` mod

main = 
  runTestTT $ TestList [
    SW ~=? opposite NE
    , N ~=? opposite S ]
