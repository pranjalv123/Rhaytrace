module Util.Camera (Camera (..)
              ,imageFromCamera
              ,getScreenPoint) where
import Util.Vector
import Util.Image

data Camera = Camera { 
  cameraPos     :: Vec
  ,cameraDir    :: Vec
  ,cameraHeight :: Int
  ,cameraWidth  :: Int
  ,focalLength  :: Double
  ,screenSize   :: (Double, Double)} deriving (Show)


getScreenPoint :: Camera -> Int -> Vec
getScreenPoint cam index = screenCenter
                           + (screenDist (unit $ cameraDir cam) pos)
  where pos = imPos index (cameraHeight cam) (cameraWidth cam)
        screenDist :: Vec -> (Int, Int) -> Vec
        screenDist normal (x, y) = 
          case normal == Vec 1 0 0 of
            True  -> Vec 0 (gridPtX x) (gridPtY y)
            False -> let m = cross (Vec 1 0 0) normal in
              ((vec1 $ gridPtX x) * m) + ((vec1 $ gridPtY y) * (cross m normal))
        --gets center of screen
        screenCenter = ((cameraPos cam) + (vec1 $ focalLength cam) * (unit $ cameraDir cam))
        --get x, y distance from origin
        gridPtX :: Int -> Double
        gridPtX x = (fromIntegral x * (fst $ screenSize cam)/(fromIntegral (cameraWidth cam)))
                    - (fst $ screenSize cam)/2
        gridPtY :: Int -> Double
        gridPtY y = (fromIntegral y * (snd $ screenSize cam)/(fromIntegral (cameraHeight cam))) 
                    - (snd $ screenSize cam)/2
                    

imageFromCamera :: Camera -> [Pixel] -> Image
imageFromCamera cam pix = Image (cameraHeight cam) (cameraWidth cam) pix

testCam = Camera (Vec 100 0 0) (Vec 0 1 0) 100 100 1 (1, 1)