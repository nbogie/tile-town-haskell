import Data.Char ( toLower )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL as GL
import Types hiding (Position)
import Parser (demoParse)
import Control.Monad 
import Data.Map as M
import Data.List (foldl')
data State = State { leftFirst :: IORef Bool }

makeState :: IO State
makeState = do
   l <- newIORef True
   return $ State { leftFirst = l }

myInit :: IO ()
myInit = do
   -- shadeModel $= Flat
   GLUT.clearColor $= Color4 0 0 0 0

drawTiles :: TileMap -> IO ()
drawTiles tmap = do
  let ts = M.assocs tmap
  forM_ ts drawTile

drawTile (p,t) = do
  GL.preservingMatrix $ do
    GL.translate $ Vector3 (0.11::GLfloat) 0.0 0
    let Grid g = tileGrid t
    forM_ (zip g [1..]) $ (\(gridLine,row) -> 
      forM_ (zip gridLine [1..]) $ (\(ter, col) -> do
        drawMyCube (col/10.0) (row/10.0) 0 (colorFor ter) 0.05
      ))

drawMyCube :: GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat -> Height -> IO ()
drawMyCube x y rot colr sz = do
  color colr
  GL.preservingMatrix $ do
      GL.scale s s s
      GL.translate $ Vector3 x y 0
      GL.rotate rot $ Vector3 1 0 0
      GLUT.renderObject GLUT.Solid (GLUT.Cube sz)
  where s = 0.1::GLfloat

colorFor Farm = Color4 0 1 0 (1 :: GLfloat)
colorFor Road = Color4 1 1 1 (1 :: GLfloat)
colorFor City = Color4 0.5 0.5 0.5 (1 :: GLfloat)
colorFor Terminus = Color4 0.5 0 0.5 (1 :: GLfloat)
colorFor Cloister = Color4 0.7 0 0 (1 :: GLfloat)

display :: TileMap -> State -> GLUT.DisplayCallback
display tiles state = do
   clear [ ColorBuffer ]
   drawTiles tiles
   flush

reshape :: GLUT.ReshapeCallback
reshape size@(GLUT.Size w h) = do
   GLUT.viewport $= (GLUT.Position 0 0, size)
   GLUT.matrixMode $= Projection
   GLUT.loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then GLUT.ortho2D 0 1 0 (hf/wf)
      else GLUT.ortho2D 0 (wf/hf) 0 1

keyboard :: State -> GLUT.KeyboardMouseCallback
keyboard state (GLUT.Char c) GLUT.Down _ _ = case toLower c of
   't'   -> do leftFirst state $~ not; GLUT.postRedisplay Nothing
   '\27' -> exitWith ExitSuccess   -- Escape key
   _     -> return ()
keyboard _ _ _ _ _ = return ()

positionTiles :: [Tile] -> TileMap
positionTiles ts = 
  foldl' f M.empty $ zip ts [1..]
  where f :: TileMap -> (Tile, Int) -> TileMap
        f tmap (t,i) = M.insert (pos ((i `mod` 10), (i `div` 10))) t tmap

-- Main Loop
-- Open window with initial window size, title bar, RGBA display mode, and
-- handle input events.
main :: IO ()
main = do
   tiles <- demoParse "../tileset.dat"
   let tileMap = positionTiles tiles
   (progName, _args) <- GLUT.getArgsAndInitialize
   initialDisplayMode $= [ GLUT.SingleBuffered, GLUT.RGBMode ]
   initialWindowSize $= Size 900 700
   createWindow progName
   state <- makeState
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   displayCallback $= display tileMap state
   mainLoop
