module Lighting.Lighting where 
import Util.Image
import Util.Vector

data Light = Ambient {intensity :: Pixel}
           | Spot {position   :: Vec
                   ,intensity :: Pixel}
             deriving (Show)
               
--lighting :: [Shape] -> [Light] -> Ray -> Pixel
--lighting h l = 