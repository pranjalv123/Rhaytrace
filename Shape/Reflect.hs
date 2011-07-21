module Shape.Reflect (specular) where
import Util.Vector
import Util.Image
import Shape.Shape
import Shape.Intersects

specular :: [Shape] -> [Light] -> Shape -> Ray -> Vector -> Pixel

specular shapes lights shape incoming point =
  let reflected = (reflectedRay shape incoming point) in 
  let hitPoint = hit shapes reflected in
  case hitPoint of
    Nothing -> (Pixel 0 0 0)
    Just x ->
      case Shape of
        (Sphere _ _ _ mat) ->
          (specular mat) * (brightness shapes lights x)
        (Plane _ _ _ mat) ->
          (specular mat) * (brightness shapes lights x)
        (Triangle _ _ _ mat) ->
          (specular mat) * (brightness shapes lights x)
        otherwise -> (Pixel 0 0 0)
  
brightness :: [Shape] -> [Light] -> (Shape, Vec)


reflectedRay :: Shape -> Ray -> Vector -> Ray