module Main where
{- 
   Uses wxHaskell bindings to the wxWidgets toolkit
-}

-- import Types
import Parser
import Graphics.UI.WX hiding (play)
import Graphics.UI.WXCore
import GUI
import System.Environment (getArgs)

main :: IO ()
main = do  
  args <- getArgs
  case args of
      [tilesFile] -> demoParse tilesFile
      _           -> start gui

