import Util.Image
import Util.Vector
import Util.Camera 
import Data.List
import Data.Maybe
import System (getArgs)
import Shape.Shape
import Shape.Intersect
import Lighting.Lighting

testRay = ray (Vec 0 0 0) (Vec 1 0 0)



traceRay :: Ray -> [Shape] -> [Light] -> Vec -> Pixel
traceRay r shapes lights dir = 
  let hitPoint = (hit shapes r) in
  pixVal lights hitPoint
  where pixVal :: [Light] -> Maybe (Shape, Vec) -> Pixel
        pixVal _ Nothing = Pixel 0 0 0
        pixVal lights (Just (s, v)) = foldl (+) 0 $ map (getLighting (s, v)) lights
        getLighting :: (Shape, Vec) -> Light -> Pixel
        getLighting (s, v) (Ambient intens) = ((color s) v) * intens
        
        getLighting (s, v) (Spot pos intens) = 
          if not (occluded v pos)
          then ((color s) v)  * (pixMap (/(dist v pos)^2) (intens))
          else (Pixel 0 0 0)
        occluded h l = case hit shapes (ray h (l - h)) of  
          Just (s, v) -> (closer h v l) == LT
          Nothing -> False
        


  
traceAll :: Camera -> [Shape] -> [Light] -> Image
traceAll camera shapes lights = imageFromCamera camera $ 
                                map (\pt -> traceRay (ray (cameraPos camera) 
                                                      (pt - (cameraPos camera)))
                                            shapes lights pt) points
                                where points = map (getScreenPoint camera)
                                               (take ((cameraHeight camera)*(cameraWidth camera))
                                                [1..])
                                      


exMat = Material (Pixel 0.7 0.7 0.7) (Pixel 0 0 0) (Pixel 0 0 0)
exSphere = Sphere (Vec 100 0 0) 3 (\_ -> Pixel 255 0 0) exMat
exSphere2 = Sphere (Vec 100 (-5) (0)) 1 (\_ -> Pixel 0 0 255) exMat 
exPlane = Plane (Vec 120 0 0) (Vec 120 0 5) (Vec 125 5 0) (\_ -> Pixel 0 255 0) exMat
exTriangle = Triangle (Vec 103 4 2) (Vec 97 1 0) (Vec 108 6 (-3)) (\_ -> Pixel 255 0 255) exMat
exAmbient = Ambient (Pixel 0.2 0.2 0.2)
exSpot = Spot (Vec 85 (-13) (3)) (Pixel 50 50 50)
exSpot2 = Spot (Vec 75 (0) (-2)) (Pixel 50 50 50)


shapes = [exSphere, exSphere2, exPlane, exTriangle]


lights = [exAmbient, exSpot, exSpot2]

c = Camera (Vec 1 0 0) (Vec 1 0 0) 200 200 1 (1, 1)

main = do
  args <- getArgs
  let cam = Camera (Vec (10) 0 0) (Vec (1) (0) (0)) 
            (read $ head args) (read $ head (tail args)) 
            10 ((2), (2))
  writeImage (traceAll cam shapes lights) "test.ppm"