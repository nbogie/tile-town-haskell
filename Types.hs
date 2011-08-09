module Types where

data Terrain = Road
             | City
             | Cloister
             | Farm
             | Terminus
  deriving (Show)

intToTerrain :: Int -> Terrain
intToTerrain 1 = City
intToTerrain 2 = Cloister
intToTerrain 3 = Farm
intToTerrain 4 = Terminus
intToTerrain 6 = Road

type Rotation = Int

data Tile = Tile { tileTemplate::TileTemplate
                 , tileRotation::Rotation } deriving (Show)

data TileTemplate = 
  TileTemplate { filename :: FilePath
               , numOccs :: Int
               , ends :: [Terrain]
               , grid :: [[Terrain]]
               } deriving (Show)

