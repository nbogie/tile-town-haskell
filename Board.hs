module Board where

import Types
import Parser
import Test.HUnit
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

isLegalPlacement :: Tile -> Position -> Board -> Bool
isLegalPlacement = undefined

-- Does no checking of game rules
place :: Tile -> Position -> Board -> Board
place t p b = Board newSlots
  where 
    slots = playedTiles b
    newSlots = 
      case M.lookup p slots of
        (Just existing) -> error $ "Already a tile placed at "++ show p
        _ -> M.insert p t slots

neighbours :: Position -> Board -> [Tile]
neighbours pos b = 
  mapMaybe (tileAt b) (adjacentPositions pos)

adjacentPositions :: Position -> [Position]
adjacentPositions p = 
  map (adjacentPosition p) directionsNESW

directionsNESW = [North .. West]

data Direction = North | East | South | West deriving (Show, Eq, Ord)
adjacentPosition :: Position -> Direction -> Position
adjacentPosition = undefined

tileAt :: Board -> Position -> Maybe Tile
tileAt p b = 
  M.lookup p playedTiles 
main = runTestTT tests

tests = TestList [allTests]
allTests = TestList [ 3 ~=? 3
                    , "foo" ~=? "bar"
                    ]

templateFromString :: String -> TileTemplate
templateFromString = undefined

template1 :: TileTemplate
template1 = parseOne ls
  where
   ls = [ "basic/bg010.png"
        , "4"
        , "1 6 6 1 "
        , ""
        , "1111111111"
        , "1111111133"
        , "1111113333"
        , "1111133333"
        , "1113333333"
        , "1113333366"
        , "1113333633"
        , "1113336633"
        , "1133363333"
        , "1333363333" ]

t1 :: Tile
t1 = tileFromTemplate template1
