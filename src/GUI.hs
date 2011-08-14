import Data.Char ( toLower )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL as GL
import Types
import Parser (demoParse)
import Control.Monad 
import Data.Map as M
import Data.List (foldl')

data State = State { leftFirst :: IORef Bool
                     , sTiles :: [Tile]
                     , floatingTile :: IORef Tile
                     , tileMap :: IORef TileMap
                     , cursorPos :: IORef Posn }

makeState :: [Tile] -> TileMap -> IO State
makeState ts tmap = do
   l  <- newIORef True
   tm <- newIORef tmap
   fTile <- newIORef $ head ts
   cpos <- newIORef $ Posn 5 5
   return $ State { leftFirst = l, tileMap = tm, floatingTile  = fTile, sTiles = ts, cursorPos = cpos}

myInit :: IO ()
myInit = do
   -- shadeModel $= Flat
   GLUT.clearColor $= Color4 0 0 0 0

drawCursor :: Posn -> IO ()
drawCursor (Posn x y) =
  drawMyCube (bip x/1.0 - 0.5) (bip y/1.0 - 0.5) 0 yellow 0.5

yellow = Color4 1 1 0 (0.5 :: GLfloat)

drawTiles :: TileMap -> IO ()
drawTiles tmap = do
  let ts = M.assocs tmap
  forM_ ts drawTile

layTile :: Posn -> Tile -> TileMap -> IO TileMap
layTile p t tmap = undefined

drawTile tpos@(Posn x y, t) = do
  GL.preservingMatrix $ do
    let Grid g = tileGrid t
    forM_ (zip g [1..]) $ (\(gridLine,row) -> 
      forM_ (zip gridLine [1..]) $ (\(ter, col) -> do
        drawMyCube (bip x + col/10.0) (bip y + row/10.0) 0 (colorFor ter) 0.05
      ))

bip :: Int -> GLfloat
bip = fromIntegral 

drawMyCube :: GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat -> Height -> IO ()
drawMyCube x y rot colr sz = do
  color colr
  GL.preservingMatrix $ do
      GL.scale s s s
      GL.translate $ Vector3 x y 0
      GL.rotate rot $ Vector3 0 0 1
      GLUT.renderObject GLUT.Solid (GLUT.Cube sz)
  where s = 0.1::GLfloat

colorFor Farm = Color4 0 1 0 (1 :: GLfloat)
colorFor Road = Color4 1 1 1 (1 :: GLfloat)
colorFor City = Color4 0.5 0.5 0.5 (1 :: GLfloat)
colorFor Terminus = Color4 0.5 0 0.5 (1 :: GLfloat)
colorFor Cloister = Color4 0.7 0 0 (1 :: GLfloat)

display :: State -> GLUT.DisplayCallback
display state = do
   clear [ ColorBuffer ]
   tmap <- readIORef $ tileMap state
   cpos <- readIORef $ cursorPos state
   fTile <- readIORef $ floatingTile state
   drawTiles tmap
   drawTile (cpos, fTile)
   drawCursor cpos
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
   '1'   -> do tileMap state $= positionTiles (take 15 $ sTiles state); GLUT.postRedisplay Nothing
   '2'   -> do tileMap state $= positionTiles (take 30 $ sTiles state); GLUT.postRedisplay Nothing
   '3'   -> do tileMap state $= positionTiles (take 45 $ sTiles state); GLUT.postRedisplay Nothing
   'i'   -> mvCursor North state
   'j'   -> mvCursor West state
   'k'   -> mvCursor South state
   'l'   -> mvCursor East state
   '\27' -> exitWith ExitSuccess   -- Escape key
   _     -> return ()
keyboard _ _ _ _ _ = return ()

mvCursor dirn state = do 
  modifyIORef (cursorPos state) (boundedStep (Bounds 0 0 20 20) dirn)
  GLUT.postRedisplay Nothing

data Bounds = Bounds Int Int Int Int deriving (Show)
boundedStep :: Bounds -> Direction -> Posn -> Posn
boundedStep bounds dirn p = 
  let p' = step dirn p
  in if inBounds p' bounds then p' else p
        where 
          inBounds (Posn x y) (Bounds x0 y0 x1 y1) = 
            x >= x0 && x <= x1 && y >= y0 && y <= y1

step North (Posn x y) = Posn x (y + 1)
step South (Posn x y) = Posn x (y - 1)
step East  (Posn x y) = Posn (x + 1) y
step West  (Posn x y) = Posn (x - 1) y

positionTiles :: [Tile] -> TileMap
positionTiles ts = 
  foldl' f M.empty $ zip ts [1..]
  where f :: TileMap -> (Tile, Int) -> TileMap
        f tmap (t,i) = M.insert (Posn (i `mod` 10) (i `div` 10)) t tmap

-- Main Loop
-- Open window with initial window size, title bar, RGBA display mode, and
-- handle input events.
main :: IO ()
main = do
   tiles <- demoParse "../tileset.dat"
   (progName, _args) <- GLUT.getArgsAndInitialize
   initialDisplayMode $= [ GLUT.SingleBuffered, GLUT.RGBMode ]
   initialWindowSize $= Size 900 700
   createWindow progName
   state <- makeState tiles M.empty
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   displayCallback $= display state
   mainLoop
