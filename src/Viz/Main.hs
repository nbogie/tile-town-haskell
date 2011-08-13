module Main where
import Data.IORef
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT

import Types
import Bindings
import Display

--
--Mostly from http://www.haskell.org/haskellwiki/OpenGLTutorial1#Hello_World

main = do
  (progname, myArgs) <- GLUT.getArgsAndInitialize
  putStrLn $ "Any extra args apart from glut ones: " ++ show myArgs
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered, GLUT.RGBAMode, 
                              GLUT.WithDepthBuffer]
  GLUT.createWindow "Tile Town Haskell"
  -- windowSize :: StateVar Size
  GLUT.windowSize $= Size 800 700
  GLUT.windowPosition $= Position 300 50
  -- GLUT.fullScreen

  md <- get maxViewportDims
  putStrLn $ "max viewport dims: "++show md

  gsRef <- newIORef initGameState -- TODO: set with window size

  GLUT.displayCallback       $= display gsRef
  GLUT.keyboardMouseCallback $= Just (myKeyMouseCallback           gsRef)
  GLUT.motionCallback        $= Just (myMouseMotionCallback        gsRef)
  GLUT.passiveMotionCallback $= Just (myPassiveMouseMotionCallback gsRef)
  GLUT.reshapeCallback       $= Just (myReshapeCallback            gsRef)
  GLUT.idleCallback          $= Just (myIdleCallback               gsRef)
  GLUT.globalKeyRepeat $= GLUT.GlobalKeyRepeatOff
  putStrLn $ reportKeyBindings keyBindings
  initDisplay
  GLUT.mainLoop
  putStrLn "Main loop exited"

