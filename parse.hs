import Data.Char (digitToInt)
main = do  
  c <- readFile "tileset.dat"
  let ts = parse c
  mapM_ print ts

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

parse :: String -> [TileTemplate]
parse "" = []
parse s = parseOne (take 14 $ lines s) : (parse (unlines (drop 14 $ lines s)))
          
parseOne ::  [String] -> TileTemplate
parseOne (fname:x:y:blank:gridlines) = TileTemplate fname (read x) (map (intToTerrain . digitToInt . head) (words y)) $ parseGrid gridlines
parseOne ls = error $ "Bad tileset data: " ++ (unlines ls)

parseGrid :: [String] -> [[Terrain]]
parseGrid gs = map (map (intToTerrain . digitToInt)) gs
