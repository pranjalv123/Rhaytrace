module Shape.Intersect (intersects, hit) where
import Util.Vector
import Shape.Shape
import Data.List
import Data.Maybe
--from http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtinter1.htm
--because I'm lazy
intersects :: Ray -> Shape -> Maybe Vec
intersects r (Sphere cent rad col _) = 
  let a = vecLen $ dir r
      b = 2 * (vecFold (+) (((orig r) - (cent)) * (dir r)))
      c = vecFold (+) (vecMap (^2) ((orig r) - (cent))) - rad^2 in
  let disc = b^2 - 4*a*c in
  if disc < 0 then Nothing else 
    let roots = ((-b - (sqrt disc)) / (2*a), 
                 (-b + (sqrt disc)) / (2*a)) in
    if (fst roots > 0.01) 
    then Just ((orig r) + (vec1 (fst roots)) * (dir r)) 
    else if (snd roots > 0.01) 
         then Just ((orig r) + (vec1 (snd roots)) * (dir r))
         else Nothing

intersects r (Triangle p1 p2 p3 c m) = 
  case intersects r (Plane p1 p2 p3 c m) of
    Nothing -> Nothing
    Just v -> if (p >= 0) && (q >= 0) && (p + q <= 1) 
              then Just v
              else Nothing
                where p = (dot11 * dot02 - dot01 * dot12) * invDenom
                      q = (dot00 * dot12 - dot01 * dot02) * invDenom
                      invDenom = 1/(dot00 * dot11 - dot01 * dot01)
                      dot00 = dot v0 v0
                      dot01 = dot v0 v1
                      dot02 = dot v0 v2
                      dot11 = dot v1 v1
                      dot12 = dot v1 v2
                      v0 = p3 - p1 
                      v1 = p2 - p1
                      v2 = v  - p2

intersects r (Plane p1 p2 p3 _ _) = 
  let n = unit $ (p2 - p1) `cross` (p3 - p1) in
  if ((dir r) `dot` n) == 0 then Nothing else 
  let k = ((p1 `dot` n) - ((orig r) `dot` n))/((dir r) `dot` n) in
  if k > 0.01 
  then Just $ (orig r) + (vec1 k) * (dir r)
  else Nothing

hit :: [Shape] -> Ray -> Maybe (Shape, Vec)
hit shapes r = case hitList of  
  [] -> Nothing 
  otherwise -> Just $ minimumBy (\ (_, x) (_, y) -> closer (orig r) x y) hitList
  where hitList =
          (map (\(a, Just b) -> (a,b)) 
           (filter (isJust . snd) 
            (zip shapes (map (intersects r) shapes))))
