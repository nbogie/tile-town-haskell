module Testing where
import Board
import Types
import Parser
import Test.HUnit
import qualified Data.Map as M
import Data.List (foldl', find)
import TestingBase
import SceneParser

testNeighbours ::  Test
testNeighbours = TestList [expected ~=? actual]
  where 
    expected = [Posn 3 2, Posn 2 3, Posn 1 2]
    actual = map (snd . fst) $ neighbours (Posn 2 2) bStart

bStart = 
  foldl' addT initBoard $ zip t1WithIds (map t2p [(1,2), (3,2), (2,3), (3,3)])
  where
    addT :: Board -> (Tile, Posn) -> Board
    addT b (t, p) = place (t, p) b

main ::  IO Counts
main = runTestTT tests

tests :: Test
tests = TestList [allTests]
allTests :: Test
allTests = TestList [ testNeighbours
                    , testFaceFromTo
                    , testBoardAccepts
                    , testFeatureOnFace
                    , testAccepts
                    , testWouldFit
                    ]

testFaceFromTo = 
  TestList [
    NorthFace ~=? faceFromTo (Posn 2 8) (Posn 2 7)
    , WestFace ~=? faceFromTo (Posn 2 8) (Posn 1 8)
    , EastFace ~=? faceFromTo (Posn 2 8) (Posn 3 8)
    , SouthFace ~=? faceFromTo (Posn 2 8) (Posn 2 9)
--    , expectError "foo" $ TestCase (faceFromTo (Posn 2 8) (Posn 3 3))
  ]

testFeatureOnFace = TestList
  [ ECity ~=? featureOnFace t NorthFace
  , ERoad ~=? featureOnFace t EastFace
  , ERoad ~=? featureOnFace t SouthFace
  , EFarm ~=? featureOnFace t WestFace
  ]
  where t = initTile {tileEndTerrains = [ECity, ERoad, ERoad, EFarm]}


testBoardAccepts = "boardAccepts" ~:
  TestList 
    [ "occupied" ~: False ~=?  boardAccepts (t1, Posn 2 3) b
    , "simpleok" ~: True ~=?  boardAccepts (mk 3 2 (replicate 4 ERoad)) bSimple 
    , "oneside" ~: True ~=?  boardAccepts (mk 3 2 [ECity,ECity, ECity, ERoad]) bSimple 
    , "oneside" ~: True ~=?  boardAccepts (mk 3 2 [ECity,ECity, ECity, ERoad]) bSimple 
    , "good" ~: True ~=?  boardAccepts (mk 2 2 [ECity, EFarm, ERoad, EFarm]) board2 
    ]
  where
    b = bStart
    bSimple = place (mk 2 2 [EFarm, ERoad, EFarm, EFarm]) initBoard
    mk x y grd = (initTile { tileEndTerrains = grd }, Posn x y)

testAccepts = "accepts" ~: 
  TestList 
  [ False ~=? accpt (mk 2 2 [ERoad, ERoad, ERoad, ERoad]) (mk 3 2 [ECity, ECity, ECity, ECity])
  , True ~=? accpt (mk 2 2 [EFarm, ERoad, EFarm, EFarm]) (mk 3 2 [ECity, ECity, ECity, ERoad])
  ]

mk x y grd = (initTile { tileEndTerrains = grd }, Posn x y)

board2 = parseSceneToBoard scene
  where
    scene = unlines
      [ "ff fr ff"
      , "ff fc rf"
      , ""
      , "ff .. ff"
      , "ff .. ff"
      ]
------- Sample Data --------------
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
t1 = tileFromTemplate template1 0

t1WithIds :: [Tile]
t1WithIds = 
  zipWith f [0..] $ cycle [t1]
    where f :: TileId -> Tile -> Tile
          f tId t = t { tileId = tId }

testWouldFit = 
  Just [Just ECity, Just EFarm, Nothing, Just EFarm] 
  ~=? wouldFit board2 (Posn 2 2)
