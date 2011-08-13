module SceneParser where
import Types
import Parser
import Board
import TestingBase
import Data.List(foldl')

parseScene :: String -> [[Maybe Tile]]
parseScene s = pOne $ lines s

pOne :: [String] -> [[Maybe Tile]]
pOne [] = error "expected first line"
pOne (l:rest) = pTwo l rest

pTwo :: String -> [String] -> [[Maybe Tile]]
pTwo l1 [] = error "expected 2nd line"
pTwo l1 (l2:rest) = pQuartets (l1,l2) : pSpacer rest

pSpacer :: [String] -> [[Maybe Tile]]
pSpacer [] = []
pSpacer ("":rest) = pOne rest

pQuartets :: (String, String) -> [Maybe Tile]
pQuartets (l1, l2) = map pQuartet $ quartets l1 l2

pQuartet :: Quartet -> Maybe Tile
pQuartet ('.', '.', '.', '.') = Nothing
pQuartet (cn,ce,cs,cw) = 
  Just $ mkTile $ map c2e [cn,ce,cs,cw]
  where
    mkTile ends = initTile {tileEndTerrains = ends }
    c2e 'f' = EFarm
    c2e 'r' = ERoad
    c2e 'c' = ECity

type Quartet = (Char,Char,Char,Char)
quartets :: String -> String -> [Quartet]
quartets l1 l2 = zipWith j (words l1) (words l2)
  where 
    j :: String -> String -> Quartet
    j [cn,ce] [cw,cs] = (cn,ce,cs,cw)
    j other1  other2  = error $ "Bad strings for quartet: " ++ show [other1, other2]
  
parseSceneToBoard :: String -> Board
parseSceneToBoard s = 
  foldl' f initBoard $ zip [1..] $ parseScene s
  where
    f ::  Board -> (Int, [Maybe Tile]) -> Board
    f b (row, scs) = foldl' (g row) b $ zip [1..] scs 
      where 
        g :: Int -> Board -> (Int, Maybe Tile) -> Board
        g row b (col, Just t) = place (t, Posn col row) b
        g _   b (_, Nothing)  = b

