module Util.Vector where

data Vec = Vec  {
    x  :: Double
    ,y :: Double
    ,z :: Double 
    } deriving (Show, Eq)

data Ray = Ray {
    orig :: Vec
    ,dir :: Vec
  } deriving (Show, Eq)

ray :: Vec -> Vec -> Ray
ray v1 v2 = Ray v1 (unit v2)

vec1 :: Double -> Vec
vec1 x = Vec x x x

dot :: Vec -> Vec -> Double
dot v1 v2 = (x v1)*(x v2) + (y v1)*(y v2) + (z v1)*(z v2)

cross :: Vec -> Vec -> Vec
cross (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

unit :: Vec -> Vec
unit v = vecMap (/(x $ abs v)) v

vecMap :: (Double -> Double) -> Vec -> Vec  
vecMap f (Vec x y z) = Vec (f x) (f y) (f z)
  
vecFold :: (Double -> Double -> Double) -> Vec -> Double
vecFold f v = f (z v) (f (x v) (y v))

vecLen :: Vec -> Double
vecLen (Vec x y z) = sqrt (x^2 + y^2 + z^2)

dist :: Vec -> Vec -> Double
dist v1 v2 = vecLen $ v2 - v1

closer :: Vec -> Vec -> Vec -> Ordering
closer compPt v1 v2 = compare (dist compPt v1) (dist compPt v2)

instance Num Vec where
  (+) (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1+x2) (y1+y2) (z1+z2)
  negate = vecMap negate
  (-) v1 v2 = v1 + (negate v2)
  (*) (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1*x2) (y1*y2) (z1*z2)
  abs v = Vec a a a where a = vecLen v
  signum = unit
  fromInteger i = Vec k k k where k = fromInteger i