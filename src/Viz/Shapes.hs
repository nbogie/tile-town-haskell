module Shapes where

import Data.IORef
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import Numeric

import Types
import Colors
import Utils

drawPinCube color = do
     GL.materialAmbient GL.FrontAndBack $= color
     GLUT.renderObject GLUT.Wireframe (GLUT.Cube 1)
     GL.preservingMatrix $  do
       GL.translate $ vc3f (-0.5) (-0.5) (-0.5)
       GL.renderPrimitive GL.Points $ 
         mapM_ (\(x,y,z) -> GL.vertex $ GL.Vertex3 x y z) gridVerts

gridVerts = [(x,y,z) | x <- steps, y <- steps, z <- steps]
  where steps :: [GL.GLfloat]
        steps = [0.0,0.1..1.0]

drawCubeTwister :: Color4 GLfloat -> Color4 GLfloat -> Int ->  IO ()
drawCubeTwister col1 col2 step = do
      GL.scale s s s 
      mapM_ dCube (zip steps colors)
  where 
    steps = map (*2) twentySteps
    rotOffset = 0
    colors = smoothColors col1 col2 (length steps)
    s = 0.6:: GLfloat
    dCube (i,c) = drawMyCube i i (i*90.0 + rotOffset) c 0.1

drawCubeLine :: Color4 GLfloat -> Color4 GLfloat -> Int ->  Bool -> IO ()
drawCubeLine col1 col2 animStep animating = do
  GL.scale s s s 
  mapM_ dCube (zip steps colors)
    where 
      s = 0.6::GLfloat
      steps = map (*2) twentyStepsInt
      colors = smoothColors col1 col2 (length steps)
      dCube (i,c) = drawMyCube (fromIntegral i/10.0) 0 0 c sz
        where sz = if animating 
                     then 0.1 + 0.05 * sin (fromIntegral (i + animStep) / 10.0)
                     else 0.1

drawCubeGrid fc animating = 
  forM_ tenSteps $ \i -> GL.preservingMatrix $ do
    GL.translate $ vc3f 0 i (-1)
    drawCubeLine (scheme 0 0) (scheme 0 2) fc animating

drawMyCube :: GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat -> Height -> IO ()
drawMyCube x y rot color sz = do
  GL.materialAmbient GL.FrontAndBack $= color
  GL.preservingMatrix $ do
      GL.translate $ Vector3 x y 0
      GL.rotate rot $ Vector3 1 0 0
      GLUT.renderObject GLUT.Solid (GLUT.Cube sz)

myPoints :: Int -> [(GLfloat,GLfloat,GLfloat)]
myPoints offset = map (\k -> (sin(offsetF + 2*pi*k/200), k/200, 0.0)) [1..200]
  where offsetF = fromIntegral offset / 100.0

