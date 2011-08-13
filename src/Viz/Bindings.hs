module Bindings where

import Types
import Display

import Data.List (find, intercalate, groupBy, sortBy, delete)
import Data.Function (on)
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef

type MyFun = IORef GameState -> IO ()
data KeyBinding = KeyBinding GLUT.Key KeyBindingCategory String MyFun

instance Show KeyBinding where
  show (KeyBinding key cat desc _) = show key ++ " -> " ++ desc

data KeyBindingCategory = KBGen | KBMov | KBDisp deriving (Eq, Ord)
instance Show KeyBindingCategory where
  show KBGen = "General"
  show KBMov = "Movement"
  show KBDisp = "Display"

reportKeyBindings :: [KeyBinding] -> String
reportKeyBindings kbsRaw = "== Key Bindings: ==\n\n" ++
                           concatMap reportKeyBindingsGroup (groupBy g kbs)
    where 
          kbs = sortBy s kbsRaw
          s (KeyBinding _ c1 _ _) (KeyBinding _ c2 _ _) = compare c1 c2
          g (KeyBinding _ c1 _ _) (KeyBinding _ c2 _ _) = c1 == c2

reportKeyBindingsGroup [] = ""
reportKeyBindingsGroup kbs@((KeyBinding _ cat _ _):_) = (show cat) ++ "\n" ++ rest
  where
    rest = (intercalate "\n" (map show kbs)) ++ "\n\n"

keyBindings ::  [KeyBinding]
keyBindings = [ 
  KeyBinding (GLUT.Char 'q') KBGen "Quit"               (\gs -> GLUT.leaveMainLoop),
  KeyBinding (GLUT.Char 'o') KBMov  "Rotate Up"         (\gs -> rotView gs rotAmt     1  0  0),
  KeyBinding (GLUT.Char 'k') KBMov  "Rotate Down"       (\gs -> rotView gs rotAmtNeg  1  0  0),
  KeyBinding  (GLUT.SpecialKey  GLUT.KeyLeft)   KBMov  "Rotate Left"   (\gs  ->  rotView  gs  rotAmtNeg  0  1  0),
  KeyBinding  (GLUT.SpecialKey  GLUT.KeyRight)  KBMov  "Rotate Right"  (\gs  ->  rotView  gs  rotAmt     0  1  0),
  KeyBinding  (GLUT.SpecialKey  GLUT.KeyUp)    KBMov  "Forward"   (\gs  ->  moveView  gs  0  0  movAmt),
  KeyBinding  (GLUT.SpecialKey  GLUT.KeyDown)  KBMov  "Backward"  (\gs  ->  moveView  gs  0  0  movAmtNeg),
  KeyBinding (GLUT.Char 'l') KBDisp  "Look at origin"    (\gs -> resetView),
  KeyBinding (GLUT.Char 'f') KBDisp "Toggle Fullscreen" (\gs -> GLUT.fullScreenToggle),
  KeyBinding (GLUT.Char '?') KBGen "Show key bindings"  (\gs -> putStrLn (reportKeyBindings keyBindings ))
 ]
  where rotAmt = 3
        rotAmtNeg = -rotAmt
        movAmt = 0.5
        movAmtNeg = - movAmt

resetView = do
  matrixMode $= Projection
  lookAt eyeV refV upV
  matrixMode $= Modelview 0
    where eyeV = Vertex3 0 0 (-2) ::Vertex3 GLdouble
          refV = Vertex3 0 0 0 ::Vertex3 GLdouble
          upV = Vector3 0 1 0 ::Vector3 GLdouble

rotView gsRef amt x y z = do
  gs <- get gsRef
  let (oldAmt,oldVec) = gsCamRot gs
  gsRef $= gs { gsCamRot = (oldAmt + amt, oldVec) }
  
moveView gsRef x y z = do
  gs <- get gsRef
  let (Vector3 x0 y0 z0) = gsCamPos gs
  gsRef $=gs { gsCamPos = Vector3 (x0+x) (y0+y) (z0+z)}


myKeyMouseCallback gameStateRef key state modifiers position = do 
  -- putStrLn $ "EVENT: " ++ show key ++ ", state: " ++ show state ++ " " ++ show modifiers

  gs <- get gameStateRef
  let heldKeysList = gsHeldKeys gs
  case find (matchBinding key) keyBindings of
    Just (KeyBinding c KBMov desc action) -> if elem c heldKeysList
                                               -- TODO: consider key state from event, too.
                                               then gameStateRef $= gs {gsHeldKeys = delete c heldKeysList }
                                               else gameStateRef $= gs {gsHeldKeys = c:heldKeysList }
    Just (KeyBinding c cat desc action) -> return ()
    _ -> return () 
  
  if state == GLUT.Down
    then
      case find (matchBinding key) keyBindings of
          Just (KeyBinding c cat desc action) -> do
            action gameStateRef
          -- here's how to do functions outwith the KeyBinding lookup
          _ -> case key of
                  GLUT.Char '1' -> GLUT.fullScreenToggle
                  GLUT.Char '2' -> rotView gameStateRef 1 0 1 0
                  _ -> return ()
    else
      return ()

matchBinding key (KeyBinding k cat desc act) = key == k

myPassiveMouseMotionCallback gameStateRef pos@(Position x y) = do
  gs <- get gameStateRef
  let (ox,oy) = gsMousePos gs
  gameStateRef $= gs {gsMousePos = (x,y) }
  GLUT.postRedisplay Nothing

myMouseMotionCallback gameStateRef pos@(Position x y) = do
  gs <- get gameStateRef
  GLUT.postRedisplay Nothing
    where fi = fromIntegral

myReshapeCallback :: (HasGetter g, HasSetter g) => g GameState -> Size -> IO ()
myReshapeCallback gsRef s@(Size w h) = do 
  gs <- get gsRef
  
  gsRef $= gs {gsSize = s}
  viewport $= (Position 0 0, s)

  matrixMode $= Projection
  loadIdentity
  perspective (fi 40) (fi w/fi h) 0.1 100 
  matrixMode $= Modelview 0
  loadIdentity
  flush
    where fi = fromIntegral

myIdleCallback gameStateRef = do
  gs <- get gameStateRef
  let c = gsFrameCount gs
  gameStateRef $= gs {gsFrameCount = c + 1} 
  GLUT.postRedisplay Nothing
