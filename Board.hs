module Board where

import Types
import Parser hiding (main)
import Test.HUnit
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (foldl', find)
isLegalPlacement :: Tile -> Position -> Board -> Bool
isLegalPlacement = undefined

-- Does no checking of game rules
place :: Tile -> Position -> Board -> Board
place t p b = Board newSlots
  where 
    slots = playedTiles b
    newSlots = 
      case M.lookup p slots of
        (Just _) -> error $ "Already a tile placed at "++ show p
        _ -> M.insert p t slots

neighbours :: Position -> Board -> [Tile]
neighbours pos b = 
  mapMaybe (tileAt b) (adjacentPositions pos)

adjacentPositions :: Position -> [Position]
adjacentPositions p = 
  map (adjacentPosition p) nesw 

adjacentPosition :: Position -> Direction -> Position
adjacentPosition (Position x y) North = Position x     (y-1)
adjacentPosition (Position x y) East  = Position (x+1) y
adjacentPosition (Position x y) South = Position x     (y+1)
adjacentPosition (Position x y) West  = Position (x-1) y

tileAt :: Board -> Position -> Maybe Tile
tileAt b p = 
  M.lookup p (playedTiles b)

main ::  IO Counts
main = runTestTT tests

tests :: Test
tests = TestList [allTests]
allTests :: Test
allTests = TestList [ testNeighbours
                    ]

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

initBoard ::  Board
initBoard = Board M.empty
t1 :: Tile
t1 = tileFromTemplate template1 0

t1WithIds :: [Tile]
t1WithIds = 
  zipWith f [0..] $ cycle [t1]
    where f :: TileId -> Tile -> Tile
          f tId t = t { tileId = tId } 

testNeighbours ::  Test
testNeighbours = TestList [expected ~=? actual]
  where 
    expected = [Position 3 2, Position 2 3, Position 1 2]
    actual = map (findTilePos bStart) $ neighbours (Position 2 2) bStart
    bStart = foldl' addT initBoard $ zip t1WithIds (map t2p [(1,2), (3,2), (2,3), (3,3)])
    addT :: Board -> (Tile, Position) -> Board
    addT b (t, p) = place t p b

findTilePos :: Board -> Tile -> Position
findTilePos b t = case find (\(_p, tOther) -> tileId t == tileId tOther) kvs of
    Just (p, _tile) -> p
    Nothing -> error $ "Did not find tile "++ show (tileId t) ++" on board."
  where kvs = M.assocs $ playedTiles b

t2p :: (Int, Int) -> Position
t2p (x,y) = Position x y
