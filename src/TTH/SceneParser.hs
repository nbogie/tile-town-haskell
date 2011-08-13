module TTH.SceneParser where
import TTH.Types
import TTH.Parser
import TTH.TestingBase
import Data.List(foldl')

type SceneComp = (Maybe Tile, Position)

parseScene :: String -> [[SceneComp]]
parseScene s = pOne 1 $ lines s

pOne :: Int -> [String] -> [[SceneComp]]
pOne i [] = error "expected first line"
pOne i (l:rest) = pTwo i l rest

pTwo :: Int -> String -> [String] -> [[SceneComp]]
pTwo i l1 [] = error "expected 2nd line"
pTwo i l1 (l2:rest) = pSceneComps i (l1,l2) : pSpacer (i+1) rest

pSpacer :: Int -> [String] -> [[SceneComp]]
pSpacer i [] = []
pSpacer i ("":rest) = pOne i rest

pSceneComps :: Int -> (String, String) -> [SceneComp]
pSceneComps i (l1, l2) = zipWith (addPos i) [1..] $ map pSceneComp $ quartets l1 l2
  where
    addPos row col tile = (tile, Position row col)

pSceneComp :: Quartet -> Maybe Tile
pSceneComp ('.', '.', '.', '.') = Nothing
pSceneComp (cn,ce,cs,cw) = 
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
  
