module Shape.Shape where
import Util.Vector
import Util.Image

data Shape = Sphere {center     :: Vec
                     ,radius    :: Double
                     ,color     :: (Vec -> Pixel)
                     ,material  :: Material
                     } 
           | Triangle {p1       :: Vec
                       ,p2      :: Vec
                       ,p3      :: Vec
                       ,color   :: (Vec -> Pixel)
                       ,material:: Material 
                       }
           | Plane {p1          :: Vec
                    ,p2         :: Vec
                    ,p3         :: Vec
                    ,color      :: (Vec -> Pixel) 
                    ,material   :: Material 
                    }
             
data Material = Material {specular     :: Pixel
                          ,diffuse     :: Pixel
                          ,translucent :: Pixel
                          }
instance Show Shape where
  show (Sphere c r col _) = show c ++ " " ++ "Rad: " ++ show r
  show (Triangle p1 p2 p3 color _) = show p1 ++ " " ++ show p2 ++ " " ++ show p3
  show (Plane p1 p2 p3 color _) = show p1 ++ " " ++ show p2 ++ " " ++ show p3


vecToSpherical (Sphere cen rad col _) v = (acos (z v')/(vecLen v'), atan2 (y v') (x v'))
  where v' = v - cen