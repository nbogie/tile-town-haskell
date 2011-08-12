module Parser where

import Data.Char (digitToInt)
import Data.List (isPrefixOf)

import Types

main = do  
  c <- readFile "tileset.dat"
  let templates = parse c
  let tiles = makeTileSet templates
  mapM_ print templates
  return tiles

parse :: String -> [TileTemplate]
parse "" = []
parse s = o : others
  where
    o      = parseOne (take 14 $ lines s)
    others = parse (unlines (drop 14 $ lines s))

parseOne ::  [String] -> TileTemplate
parseOne (fname:x:y:info:gridlines) = 
  TileTemplate fname 
               (read x)
               specialM
               (map (intToEndTerrain . digitToInt . head) (words y))
               (parseGrid gridlines)
  where 
    specialM | "Pennant" `isPrefixOf` info = Just Pennant
             | "Start" `isPrefixOf` info = Just Start
             | otherwise                 = Nothing

parseOne ls = error $ "Bad tileset data: " ++ (unlines ls)

parseGrid :: [String] -> [[Terrain]]
parseGrid gs = map (map (intToTerrain . digitToInt)) gs

makeTileSet :: [TileTemplate] -> [Tile]
makeTileSet = concatMap one
  where one :: TileTemplate -> [Tile]
        one template = replicate (numOccs template) $ tileFromTemplate template
