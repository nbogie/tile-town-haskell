module Parser where

import Data.Char (digitToInt)
import Data.List (isPrefixOf)

import Types

demoParse :: FilePath -> IO [Tile]
demoParse fname = do
  c <- readFile fname
  let templates = parse c
  let tiles = makeTileSet templates
  mapM_ print templates
  return tiles

usage :: String
usage = "prog path_to_tileset_file"

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

parseOne ls = error $ "Bad tileset data: " ++ unlines ls

parseGrid :: [String] -> Grid
parseGrid = Grid . map (map (intToTerrain . digitToInt))

makeTileSet :: [TileTemplate] -> [Tile]
makeTileSet templates = m templates 0
  where 
    m :: [TileTemplate] -> TileId -> [Tile]
    m [] _ = []
    m (tmpl:rest) n = map (tileFromTemplate tmpl) [n .. (n + numOccs tmpl)] 
                      ++ m rest (n + numOccs tmpl + 1)
