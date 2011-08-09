module Parser where

import Data.Char (digitToInt)
import Types

main = do  
  c <- readFile "tileset.dat"
  let ts = parse c
  mapM_ print ts

parse :: String -> [TileTemplate]
parse "" = []
parse s = parseOne (take 14 $ lines s) : (parse (unlines (drop 14 $ lines s)))
          
parseOne ::  [String] -> TileTemplate
parseOne (fname:x:y:blank:gridlines) = TileTemplate fname (read x) (map (intToTerrain . digitToInt . head) (words y)) $ parseGrid gridlines
parseOne ls = error $ "Bad tileset data: " ++ (unlines ls)

parseGrid :: [String] -> [[Terrain]]
parseGrid gs = map (map (intToTerrain . digitToInt)) gs
