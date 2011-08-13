module Types where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT

type MyPos = (GLint, GLint)
data GameState = GameState { 
                             gsHeldKeys :: [GLUT.Key],
                             gsMousePos :: MyPos, 
                             gsFrameCount :: Int, 
                             gsCamPos :: Vector3 GLfloat,
                             gsCamRot :: (GLfloat, Vector3 GLfloat),
                             gsSize :: Size
                           }
initGameState = GameState { gsHeldKeys = [],
                            gsMousePos = (0,0)::MyPos, 
                            gsCamPos = Vector3 0 0 (-2),
                            gsCamRot = (0, Vector3 0 0 0),
                            gsFrameCount = 0, 
                            gsSize = Size 10 10 }

