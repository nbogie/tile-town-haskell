module Main where

import TTH.Types
import TTH.Parser

import System.Environment (getArgs)

main = do  
  args <- getArgs
  case args of
      [tilesFile] -> demoParse tilesFile
      _           -> error usage

