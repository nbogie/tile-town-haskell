module Colors where

import Graphics.Rendering.OpenGL as GL

white :: Color4 GLfloat
white        =  Color4     1    1    1    1.0
red          =  Color4     1    0    0    1.0
colGreen :: Color4 GLfloat
colGreen     =  Color4     0    1    0    1.0
colYellow :: Color4 GLfloat
colYellow    =  Color4     1    1    0    1.0
blue         =  Color4     0    0    1    1.0
lightYellow  =  Color4     0.3  1    1    1.0
softGreen    =  Color4     0    0.5  0.3  1.0

colBlue :: Color4 GLfloat
colBlue = Color4 0.00 0.07 0.67 1.0
colOrange :: Color4 GLfloat
colOrange = Color4 1.00 0.33 0.00 1.0
colDarkGray :: Color4 GLfloat
colDarkGray = Color4 0.40 0.40 0.33 1.0

scheme 0 0 = Color4 0.00 0.07 0.67 1.0
scheme 0 1 = Color4 0.40 0.40 0.33 1.0
scheme 0 2 = Color4 1.00 0.33 0.00 1.0
scheme 0 3 = Color4 0.60 0.60 0.53 1.0

scheme 1 0 = Color4 0.73 0.95 0.00 1.0
scheme 1 1 = Color4 0.93 0.00 0.18 1.0
scheme 1 2 = Color4 0.37 0.05 0.67 1.0
scheme 1 3 = Color4 0.47 0.62 0.00 1.0

smoothColors :: (Fractional a, Num a) => 
                Color4 a -> Color4 a -> Int -> [Color4 a]
smoothColors c1 _ 1 = [c1]
smoothColors c1@(Color4 r1 g1 b1 a1) c2@(Color4 r2 g2 b2 a2) numSteps = 
  let n = numSteps - 1
      dr = r2 - r1
      dg = g2 - g1
      db = b2 - b1
      da = a2 - a1
      steps = [fromIntegral v/fromIntegral n | v <- [0..n] ]
      makeStep d = Color4 (r1 + d * dr) 
                          (g1 + d * dg) 
                          (b1 + d * db) 
                          (a1 + d * da) 
  in map makeStep steps
