import Text.Printf
import Numeric

main = interact f
f :: String -> String
f ls = unlines $ map convertHexStr $ zip (words ls) [0..]
  where 
        convertHexStr :: (String, Int) -> String
        convertHexStr ([r1,r2,g1,g2,b1,b2], i) = 
          let rgb = [conHex [r1,r2], conHex [g1,g2], conHex [b1,b2]]
              [r,g,b] = map scl rgb
          in printf "pColor %d = Color4 %0.2f %0.2f %0.2f 1.0" i r g b
            where 
                  scl :: Integer -> Double
                  scl v = (fromIntegral v) / 255.0

conHex :: String -> Integer
conHex hs = case readHex hs of
            ((n,_):_) -> n
            _         -> error $ "bad hex val: " ++ hs 
