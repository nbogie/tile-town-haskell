module Display (display, initDisplay) where

import Data.IORef
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Control.Monad (forM_)

import Types
import Colors
import Utils
import Shapes

lightDiffuse :: Color4 GLfloat
lightDiffuse  =  Color4  0.2  0.2  0.2  1.0
lightAmbient = white

lightPosition :: Vertex4 GLfloat
lightPosition = Vertex4 10.0 20.0 (-30.0) 0.0

initDisplay = do
     let light0 = Light 0 
     GL.blend $= GL.Enabled
     GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
     -- GL.lineSmooth $= GL.Enabled
     GL.pointSmooth $= GL.Enabled
     GL.polygonSmooth $= GL.Enabled
     GL.depthFunc $= Just Lequal
     GL.lineWidth $= 1
     GL.pointSize $= 1
     diffuse light0 $= lightDiffuse
     ambient light0 $= lightAmbient
     position light0 $= lightPosition
     light light0 $= Enabled
     lighting $= Enabled

display gameStateRef = do
     gs <- get gameStateRef
     let a@(mouseX,mouseY) = gsMousePos gs
     let fc = gsFrameCount gs
     let sz@(Size w h) = gsSize gs
     let camPos = gsCamPos gs
     let (camRotAmt, camRotVec) = gsCamRot gs
     let (mouseX01, mouseY01) = convertMouse a sz

     withProjection $ do
       loadIdentity
       perspective (fromIntegral 40) (fromIntegral w/fromIntegral h) 0.1 100 
       GL.translate (gsCamPos gs)
       GL.rotate camRotAmt (Vector3 0 1 0)

     GLUT.reportErrors
     GL.clearColor $= GL.Color4 0 0 0 1
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]
     loadIdentity

     drawCubeGrid fc True
     off $ drawCubeTwister colBlue colOrange fc
     off $ drawPinCube colOrange

     GL.flush
     GLUT.swapBuffers

on f = f
off f = return ()
