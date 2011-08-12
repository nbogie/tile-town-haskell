module Types where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as M

data Position = Position Int Int deriving (Show, Ord, Eq)

data Face = NorthFace 
          | EastFace 
          | SouthFace 
          | WestFace 
  deriving (Show, Eq, Ord)

data Terrain = Road
             | City
             | Cloister
             | Farm
             | Terminus
  deriving (Show, Eq)

data EndTerrain = ERoad 
                | ECity 
                | EFarm 
  deriving (Show, Eq)

intToTerrain :: Int -> Terrain
intToTerrain 1 = City
intToTerrain 2 = Cloister
intToTerrain 3 = Farm
intToTerrain 4 = Terminus
intToTerrain 6 = Road

intToEndTerrain :: Int -> EndTerrain
intToEndTerrain i = 
  case intToTerrain i of
    City -> ECity
    Farm -> EFarm
    Road -> ERoad
    _    -> error $ "Invalid EndTerrain int: " ++ (show i)

type Rotation = Int

data Tile = 
  Tile { tileTemplate::TileTemplate
       , tileEndTerrains::[EndTerrain]
       , tileRotation::Rotation
       } deriving (Show)

data Special = Pennant | Start deriving (Show, Eq)

data TileTemplate = 
  TileTemplate { filename :: FilePath
               , numOccs :: Int
               , special :: Maybe Special
               , tileTemplateTerrains :: [EndTerrain]
               , grid :: [[Terrain]]
               } deriving (Show)

tileHasPennant :: TileTemplate -> Bool
tileHasPennant tmpl = special tmpl == Just Pennant
tileIsStart tmpl = special tmpl == Just Start

initRotation :: Rotation
initRotation = 0

data RotationDir = CW | CCW deriving (Show, Eq)

rotateElems :: [a] -> RotationDir -> [a]
rotateElems [] _ = []
rotateElems [t] _ = [t]
rotateElems (t:rest) CCW = rest ++ [t]
rotateElems ts        CW = last ts : init ts

tileFromTemplate :: TileTemplate -> Tile
tileFromTemplate template = 
  Tile template (tileTemplateTerrains template) initRotation

data Board = Board { playedTiles :: M.Map Position Tile } deriving (Show)

data Game = Game { gameBoard :: Board
                 , gamePlayers :: [Player]
                 , remainingTiles :: [Tile]
                 }

data Player = Player String deriving (Show, Eq, Ord)

featureOnFace :: Tile -> Face -> EndTerrain
featureOnFace t f = 
  (tileEndTerrains t) !! (faceIndex f)

-- TODO: avoid use of fromJust
faceIndex :: Face -> Int
faceIndex f =fromJust $ elemIndex f [NorthFace, EastFace, SouthFace, WestFace]
