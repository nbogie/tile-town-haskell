module TestingBase where
import Types
import Board
import qualified Data.Map as M
initTile ::  Tile
initTile = 
  Tile 0 
       (tileTemplateTerrains initTileTemplate)
       0
       (tileTemplateGrid initTileTemplate)

initTileTemplate = 
  TileTemplate "fname"
               3
               (Just Pennant)
               [ECity, ERoad, ERoad, EFarm] 
               (Grid [[]])
               
