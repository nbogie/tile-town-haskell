module Paths_tile_town_haskell where

getDataFileName :: FilePath -> IO FilePath
getDataFileName fp = return ("../" ++ fp)
