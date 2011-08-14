import Data.Char ( toLower )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef, writeIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL as GL
import Types
import Board
import Parser (demoParse)
import Control.Monad 
import Data.Map as M
import Data.List (foldl')
import Shuffle

-- Notes: $~ is modifyIORef shorthand

data State = State { leftFirst :: IORef Bool
                     , sTiles :: IORef [Tile]
                     , floatingTile :: IORef Tile
                     , board :: IORef Board
                     , relaxRules :: IORef Bool
                     , cursorPos :: IORef Posn }

makeState :: [Tile] -> TileMap -> IO State
makeState ts tmap = do
   l  <- newIORef True
   b <- newIORef (Board tmap)
   rFTile <- newIORef $ head ts
   rCPos <- newIORef $ Posn 5 5
   rTiles <- newIORef ts
   rRelaxRules <- newIORef False
   return $ State { leftFirst = l
                  , board = b
                  , floatingTile  = rFTile
                  , sTiles = rTiles
                  , relaxRules = rRelaxRules
                  , cursorPos = rCPos}

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

uiRotateTile :: State -> IO ()
uiRotateTile state = do
  (floatingTile state) $~ (rotateTile Types.CCW)
  GLUT.postRedisplay Nothing

infoAtCursor :: State -> IO ()
infoAtCursor state = do
  cpos <- readIORef $ cursorPos state
  b <- readIORef $ board state
  case tileAndPosAt b cpos of
    Just (t,_p) -> putStrLn $ show t ++ " at " ++ show cpos
    Nothing -> putStrLn $ "Nothing at " ++ show cpos
  putStrLn $ "Neighbours: \n\t" ++ show (neighbours cpos b)

layTile :: State -> IO ()
layTile state = do
  cpos <- readIORef $ cursorPos state
  fTile <- readIORef $ floatingTile state
  ts <- readIORef $ sTiles state
  b <- readIORef $ board state
  relaxed <- readIORef $ relaxRules state
  putStrLn $ "Floating tile: " ++ show fTile
  if M.null (playedTiles b) || relaxed || boardAccepts (fTile, cpos) b
    then
      case (place (fTile, cpos)) b of
        Left err -> putStrLn $ "UIFeedback: Already a piece at " ++ show cpos
        Right b' -> do
          putStrLn $ "Laying tile at " ++ show cpos
          writeIORef (board state) b'
          writeIORef (floatingTile state) (head ts)
          (sTiles state) $~ tail
    else
      putStrLn $ "Board does not accept piece at: "++ show cpos
  GLUT.postRedisplay Nothing

drawTile tpos@(Posn x y, t) = do
  GL.preservingMatrix $ do
    let Grid g = tileGrid t
    forM_ (zip (reverse g) [1..]) $ (\(gridLine,row) -> 
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
   b <- readIORef $ board state
   let tmap = playedTiles b
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
   '1'   -> showMore 15 state
   '2'   -> showMore 30 state
   '3'   -> showMore 45 state
   '4'   -> showMore 60 state
   'e'   -> relaxRules state $~ not
   'i'   -> mvCursor North state
   'j'   -> mvCursor West state
   'k'   -> mvCursor South state
   'l'   -> mvCursor East state
   'z'   -> infoAtCursor state
   'd'   -> layTile state
   'r'   -> uiRotateTile state
   '\27' -> exitWith ExitSuccess   -- Escape key
   _     -> return ()
keyboard _ _ _ _ _ = return ()

showMore n state = do 
  ts <- readIORef (sTiles state)
  board state $= positionTiles (take n ts)
  GLUT.postRedisplay Nothing

mvCursor dirn state = do 
  (cursorPos state) $~ (boundedStep (Bounds 0 0 20 20) dirn)
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

positionTiles :: [Tile] -> Board
positionTiles ts = 
  Board $ foldl' f M.empty $ zip ts [1..]
  where f :: TileMap -> (Tile, Int) -> TileMap
        f tmap (t,i) = M.insert (Posn (i `mod` 10) (i `div` 10)) t tmap

-- Main Loop
-- Open window with initial window size, title bar, RGBA display mode, and
-- handle input events.
main :: IO ()
main = do
   tilesSorted <- demoParse "../tileset.dat"
   tiles <- shuffle tilesSorted
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
