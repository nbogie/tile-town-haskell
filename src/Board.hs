module Board where

import Types
import Parser
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (foldl', find)
isLegalPlacement :: Tile -> Posn -> Board -> Bool
isLegalPlacement = undefined

-- Does no checking of game rules
place :: (Tile, Posn) -> Board -> Either String Board
place (t, p) b = fmap Board newSlots
  where 
    slots = playedTiles b
    newSlots = 
      case M.lookup p slots of
        (Just _) -> Left $ "Already a tile placed at "++ show p
        _ -> Right $ M.insert p t slots

placeOrFail tpos b = 
  case place tpos b of
    Left e -> error e
    Right b -> b

neighbours :: Posn -> Board -> [Neighbour]
neighbours p b = 
  mapMaybe f (adjacentPosnsAnnotated p)
    where 
      f (pAdj,dir) = 
        case tileAndPosAt b pAdj of
          Just tpos -> Just (tpos, dir)
          Nothing -> Nothing 

adjacentPosnsAnnotated :: Posn -> [(Posn, Direction)]
adjacentPosnsAnnotated p = zip (adjacentPosns p) nesw

adjacentPosns :: Posn -> [Posn]
adjacentPosns p = 
  map (adjacentPosn p) nesw 

adjacentPosn :: Posn -> Direction -> Posn
adjacentPosn (Posn x y) North = Posn x     (y-1)
adjacentPosn (Posn x y) East  = Posn (x+1) y
adjacentPosn (Posn x y) South = Posn x     (y+1)
adjacentPosn (Posn x y) West  = Posn (x-1) y

tileAndPosAt :: Board -> Posn -> Maybe TPos
tileAndPosAt b p = case hasTileAt b p of
  Just t -> Just (t, p)
  Nothing -> Nothing

hasTileAt :: Board -> Posn -> Maybe Tile
hasTileAt b p = 
  M.lookup p (playedTiles b)

-- findTilePos :: Board -> Tile -> Posn
-- findTilePos b t = case find (\(_p, tOther) -> tileId t == tileId tOther) kvs of
--    Just (p, _tile) -> p
--    Nothing -> error $ "Did not find tile "++ show (tileId t) ++" on board."
--  where kvs = M.assocs $ playedTiles b

t2p :: (Int, Int) -> Posn
t2p (x,y) = Posn x y

boardAccepts :: (Tile, Posn) -> Board -> Bool
boardAccepts (t,p) b =
    emptySpot 
    && hasANeighbour 
    && sidesMatchOnAll
  where
    emptySpot = Nothing == M.lookup p (playedTiles b)
    ns :: [(TPos, Direction)]
    ns = neighbours p b
    hasANeighbour = not $ null ns
    sidesMatchOnAll = all (accpt (t,p)) (map fst ns)

accpt :: TPos -> TPos -> Bool
accpt (t0, p0) (t1, p1) = accepts (t0, faceFromTo p0 p1) t1

areAdjacent :: Posn -> Posn -> Bool
areAdjacent p0 p1 = p0 `elem` adjacentPosns p1

-- which face on the first tile faces the second tile
faceFromTo :: Posn -> Posn -> Face
faceFromTo p0@(Posn x0 y0) p1@(Posn x1 y1) 
  | x1 == x0     && y1 == y0 - 1 = NorthFace
  | x1 == x0     && y1 == y0 + 1 = SouthFace
  | x1 == x0 + 1 && y1 == y0     = EastFace
  | x1 == x0 - 1 && y1 == y0     = WestFace
  | otherwise = error $ "faceFromTo positions must be adjacent but were " ++ show [p0,p1]

wouldFit :: Board -> Posn -> Maybe [Maybe EndTerrain]
wouldFit b p = 
  if emptySpot && hasANeighbour
    then Just $ makeMatchingEndsFor ns
    else Nothing
  where
    emptySpot = Nothing == M.lookup p (playedTiles b)
    ns = neighbours p b
    hasANeighbour = not $ null ns

neighboursNESW :: [Neighbour] -> [Maybe Neighbour]
neighboursNESW ns = 
  map findit nesw
    where
      findit :: Direction -> Maybe Neighbour
      findit d = find (\(tpos, ndir) -> ndir == d) ns

makeMatchingEndsFor :: [Neighbour] -> [Maybe EndTerrain]
makeMatchingEndsFor ns = map (fmap featureOnNeighbour) $ neighboursNESW ns

featureOnNeighbour ((t, p), d) = featureOnFace t f
  where f = opposite $ dir2Face d

dir2Face :: Direction -> Face
dir2Face d = toEnum (fromEnum d)

initBoard ::  Board
initBoard = Board M.empty
