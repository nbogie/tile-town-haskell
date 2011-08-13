module TTH.Board where

import TTH.Types
import TTH.Parser
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (foldl', find)
isLegalPlacement :: Tile -> Position -> Board -> Bool
isLegalPlacement = undefined

-- Does no checking of game rules
place :: (Tile, Position) -> Board -> Board
place (t, p) b = Board newSlots
  where 
    slots = playedTiles b
    newSlots = 
      case M.lookup p slots of
        (Just _) -> error $ "Already a tile placed at "++ show p
        _ -> M.insert p t slots

neighbours :: Position -> Board -> [TPos]
neighbours pos b = 
  mapMaybe (tileAndPosAt b) (adjacentPositions pos)

adjacentPositions :: Position -> [Position]
adjacentPositions p = 
  map (adjacentPosition p) nesw 

adjacentPosition :: Position -> Direction -> Position
adjacentPosition (Position x y) North = Position x     (y-1)
adjacentPosition (Position x y) East  = Position (x+1) y
adjacentPosition (Position x y) South = Position x     (y+1)
adjacentPosition (Position x y) West  = Position (x-1) y

tileAndPosAt :: Board -> Position -> Maybe TPos
tileAndPosAt b p = case tileAt b p of
  Just t -> Just (t, p)
  Nothing -> Nothing

tileAt :: Board -> Position -> Maybe Tile
tileAt b p = 
  M.lookup p (playedTiles b)

findTilePos :: Board -> Tile -> Position
findTilePos b t = case find (\(_p, tOther) -> tileId t == tileId tOther) kvs of
    Just (p, _tile) -> p
    Nothing -> error $ "Did not find tile "++ show (tileId t) ++" on board."
  where kvs = M.assocs $ playedTiles b

t2p :: (Int, Int) -> Position
t2p (x,y) = Position x y

boardAccepts :: (Tile, Position) -> Board -> Bool
boardAccepts (t,p) b = do
    emptySpot 
    && hasANeighbour 
    && sidesMatchOnAll
  where
    emptySpot = Nothing == M.lookup p (playedTiles b)
    ns = neighbours p b
    hasANeighbour = not $ null ns
    sidesMatchOnAll = and $ map (accpt (t,p)) ns

accpt :: TPos -> TPos -> Bool
accpt (t0, p0) (t1, p1) = accepts (t0, faceFromTo p0 p1) t1

areAdjacent :: Position -> Position -> Bool
areAdjacent p0 p1 = p0 `elem` adjacentPositions p1

-- which face on the first tile faces the second tile
faceFromTo :: Position -> Position -> Face
faceFromTo p0@(Position x0 y0) p1@(Position x1 y1) 
  | x1 == x0     && y1 == y0 - 1 = NorthFace
  | x1 == x0     && y1 == y0 + 1 = SouthFace
  | x1 == x0 + 1 && y1 == y0     = EastFace
  | x1 == x0 - 1 && y1 == y0     = WestFace
  | otherwise = error $ "faceFromTo positions must be adjacent but were " ++ show [p0,p1]
