module Types where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as M

data Direction = North | East | South | West deriving (Show, Eq, Ord, Enum, Bounded)
directionsNESW = [North .. West]
facesNESW = [NorthFace .. WestFace]

data Position = Position Int Int deriving (Show, Ord, Eq)

data Face = NorthFace 
          | EastFace 
          | SouthFace 
          | WestFace 
  deriving (Show, Eq, Ord, Enum, Bounded)

data Terrain = Road
             | City
             | Cloister
             | Farm
             | Terminus
  deriving (Show, Eq, Ord)

data EndTerrain = ERoad 
                | ECity 
                | EFarm 
  deriving (Show, Eq, Ord)

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
    _    -> error $ "Invalid EndTerrain int: " ++ show i

type Rotation = Int

data Tile = 
  Tile { tileId :: Int
       , tileTemplate::TileTemplate
       , tileEndTerrains::[EndTerrain]
       , tileRotation::Rotation
       } deriving (Show, Eq, Ord)

data Special = Pennant | Start deriving (Show, Eq, Ord)

data TileTemplate = 
  TileTemplate { filename :: FilePath
               , numOccs :: Int
               , special :: Maybe Special
               , tileTemplateTerrains :: [EndTerrain]
               , grid :: [[Terrain]]
               } deriving (Show, Eq, Ord)

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

type TileId = Int

tileFromTemplate :: TileTemplate -> TileId -> Tile
tileFromTemplate template tId = 
  Tile tId template (tileTemplateTerrains template) 
       initRotation

data Board = Board { playedTiles :: M.Map Position Tile } deriving (Show)

data Game = Game { gameBoard :: Board
                 , gamePlayers :: [Player]
                 , remainingTiles :: [Tile]
                 }

data Player = Player String deriving (Show, Eq, Ord)

featureOnFace :: Tile -> Face -> EndTerrain
featureOnFace t f = tileEndTerrains t !! fromEnum f

accepts :: Tile -> Face -> Tile -> Bool
accepts t f otherT = endT == otherEndT
  where 
    endT = featureOnFace t f
    otherEndT = featureOnFace otherT (opposite f)

next :: (Enum a, Bounded a) => a -> a
next = turn 1

prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

-- TODO: this is only correct for even-element enumerations
opposite :: (Enum a, Bounded a) => a -> a
opposite e = turn halfNum e
  where halfNum = 1 + fromEnum (maxBound `asTypeOf` e) `div` 2

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum $ add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n
    where
      add mod x y = (x + y + mod) `rem` mod
