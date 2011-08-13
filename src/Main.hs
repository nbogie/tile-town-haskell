module Main where
{- 
   Uses wxHaskell bindings to the wxWidgets toolkit
-}

-- import Types
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do  
  args <- getArgs
  case args of
      [tilesFile] -> demoParse tilesFile
      _           -> error "pass tileset filename"
  return ()
