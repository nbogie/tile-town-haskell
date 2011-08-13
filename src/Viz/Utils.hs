module Utils where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT

withProjection :: IO () -> IO() 
withProjection action = do
  matrixMode $= Projection
  action
  matrixMode $= Modelview 0

clamp :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat 
clamp mx mn mx' mn' x = (x-mn) / (mx-mn) * (mx'-mn') + mn'


vc3f x y z = Vector3 x y z :: Vector3 GLfloat
vx3f x y z = Vertex3 x y z :: Vertex3 GLfloat

convertMouse :: (GLint, GLint) -> Size -> (GLfloat, GLfloat)
-- convertMouse (mx, my) = (clamp (fromIntegral mx) / 800.0, - (fromIntegral my) / 600.0)::(GLfloat, GLfloat)
convertMouse (mx, my) (Size w h) = 
  (clamp 0 (fromIntegral w) (-1) 1 mxf, clamp 0 (fromIntegral h) 1 (-1) myf)
    where mxf = fromIntegral mx::GLfloat
          myf = fromIntegral my::GLfloat

tenSteps = [-0.5, -0.4 .. 0.5]::[GLfloat]
twentySteps = [-1.0, -0.9 .. 1.0]::[GLfloat]
twentyStepsInt = [-10, -9 .. 10]::[Int]
