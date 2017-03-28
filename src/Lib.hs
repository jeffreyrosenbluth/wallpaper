module Lib where

import Codec.Picture
import Codec.Picture.Types
import Data.Complex
import System.Environment

type Recipe = Complex Double -> Complex Double

scaleZ :: RealFloat a => a -> Complex a -> Complex a
scaleZ k z = (k :+ 0) * z

(.*^) :: RealFloat a => a -> Complex a -> Complex a
(.*^) = scaleZ

infixl 7 .*^

getPixel :: Image PixelRGBA8 -> Double -> Recipe -> Int -> Int -> PixelRGBA8
getPixel img s rcp i j = clamp i' j'
  where
    (w, h) = (imageWidth img, imageHeight img)
    (w2, h2) = (fromIntegral w / 2, fromIntegral h / 2)
    -- Center the coordingates, then apply the symmetry recipe and scale by s.
    -- Instead of scaling the image up by 's', we scale the point z down by s.
    (x :+ y) = (1/s) .*^ rcp ((fromIntegral i - w2) :+ (fromIntegral j - h2))
    -- recenter the coordinates so that (0,0) is in the center of the original image.
    (i', j') = (round x + w `div` 2, round y + h `div` 2)
    -- If either coordinate is out of the bounds of the original image, color it black.
    clamp m n = if m < 0 || n < 0 || m >= w || n >= h
                  then PixelRGBA8 0 0 0 255 -- opaque black
                  else pixelAt img m n

transform :: Double -> Recipe -> Image PixelRGBA8 -> Image PixelRGBA8
transform s rcp img = generateImage (getPixel img s rcp) w h
  where
    (w, h) = (imageWidth img, imageHeight img)

recipe5 :: Complex Double -> Complex Double -> Recipe
recipe5 a b z = z**5 + z'**5 + a * (z**6 * z' + z * z'**6)  + b * (z**4 * z'**(-6) + z**(-6) * z'**4)
  where
    z' = conjugate z

run :: IO ()
run = do
  dImg <- readImage . head =<< getArgs
  let tr = transform (10**11) (recipe5 (0.003 :+ 0) (10**18 :+ 0))
  writePng "output.png" $ case dImg of
    Left msg -> error msg
    Right (ImageRGB8  img) -> tr (promoteImage img)
    Right (ImageRGBA8 img) -> tr img
    Right _                -> error "png images only please."
