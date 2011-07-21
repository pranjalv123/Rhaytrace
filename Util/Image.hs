module Util.Image (Image(..), 
                   Pixel(..),
                   pix,
                   imIndex,
                   imPos,
                   writeImage,
                   pixelFromDouble,
                   pixMap) where
import System.IO
import Data.List
import Util.Vector


data Image = Image {
     height     :: Int
     ,width     :: Int
     ,pixels    :: [Pixel] --pixels go left to right, top to bottom
  } deriving (Show)
data Pixel = Pixel {
     r  :: Double
     ,g :: Double
     ,b :: Double
  } deriving (Eq)


instance Show Pixel where 
  show p = (show $ round (r p)) ++ " " ++ (show $ round (g p)) ++ " " ++ (show $ round (b p))


instance Num Pixel where
  (+) (Pixel x1 y1 z1) (Pixel x2 y2 z2) = Pixel (x1+x2) (y1+y2) (z1+z2)
  negate (Pixel r g b) = Pixel (-r) (-g) (-b)
  (-) v1 v2 = v1 + (negate v2)
  (*) (Pixel x1 y1 z1) (Pixel x2 y2 z2) = Pixel (x1*x2) (y1*y2) (z1*z2)
  abs v = v
  signum p = 1
  fromInteger i = Pixel k k k where k = fromInteger i


pixelFromDouble :: Double -> Pixel
pixelFromDouble i = Pixel i i i

pixMap f (Pixel a b c) = Pixel (f a) (f b) (f c)


pix :: (Int, Int) -> Image -> Pixel
pix (x, y) im = (pixels im) !! imIndex (x, y) im

imIndex :: (Int, Int) -> Image -> Int
imIndex (x, y) im = (x * (width im) + y)

imPos :: Int -> Int -> Int -> (Int, Int)
imPos loc height width = (loc `div` width, loc `mod` width)

imageToPPM :: Image -> String
imageToPPM image = 
  "P3\n" 
  ++ (show $ width image) ++ " "
  ++ (show $ height image) ++ "\n"
  ++ "256\n"
  ++ foldr (\p -> (++) ("\n" ++p)) "" (map show 
      (pixels image)) 

writeImage image filename = writeFile filename (imageToPPM image)
{--
testImage w h = Image w h (map (\l -> testColor $ imPos l w h)  [1..(w*h)])
  where testColor (x, y) = Pixel (x*256 `div` w) (y*256 `div` h) ((w + h) - x - y)
        
        
simpleTestImage = Image 100 100 (take 10000 $ cycle [Pixel 100 100 100])
write = writeImage (testImage 1024 768) "test.ppm"--}
