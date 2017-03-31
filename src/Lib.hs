module Lib where

import           Types
import           Complextra
import           Recipes.Wallpaper

import           Codec.Picture
import           Codec.Picture.Types
import           Data.Complex
import           System.Environment

focus :: Double -> Double -> Recipe  -> Recipe
focus s t rcp (x :+ y) = s * t .*^ rcp (x / t :+ y / t)

getPixel :: Image PixelRGBA8 -> Double -> Double -> Recipe -> Int -> Int -> PixelRGBA8
getPixel img s t rcp i j = clamp i' j'
  where
    (w, h) = (imageWidth img, imageHeight img)
    (w2, h2) = (fromIntegral w / 2, fromIntegral h / 2)
    (x :+ y) = focus (1/s) t rcp ((fromIntegral i - w2) :+ (fromIntegral j - h2))
    (i', j') = (round x + w `div` 2, round y + h `div` 2)
    clamp m n = if m < 0 || n < 0 || m >= w || n >= h
                  then PixelRGBA8 0 0 0 255 -- opaque black
                  else pixelAt img m n

transform :: Double -> Recipe -> Image PixelRGBA8 -> Image PixelRGBA8
transform s rcp img = generateImage (getPixel img s (fromIntegral w / 8) rcp) w h
  where
    (w, h) = (imageWidth img, imageHeight img)

recipe5 :: Complex Double -> Complex Double -> Recipe
recipe5 a b z = z**5 + z'**5
              + a * (z**6 * z' + z * z'**6)
              + b * (z**4 * z'**(-6) + z**(-6) * z'**4)
  where
    z' = conjugate z

run :: IO ()
run = do
  dImg <- readImage . head =<< getArgs
  -- let tr = transform (10**11) (recipe5 (0.003 :+ 0) (0 :+ 0)) --(10**18 :+ 0))
  -- let tr = transform  1 p4m
  -- let tr = transform 7 (p1 0.5 0.5 [((1,0),1.5:+1), ((0,1), (1):+(-1.5)), ((1,(-1)),2:+2)])
  -- let tr = transform 7 (p2 1 1 [((1,0),1.5:+1), ((0,1), (1):+(-1.5)), ((1,(-1)),2:+2)])
  -- let tr = transform 7 (cm 1 [((1,0),1.5:+1), ((3,1), 2:+(-1.5)), ((1,(-1)),2:+1)])
  -- let tr = transform 7 (cmm 1 [((1,0),1.5:+1), ((3,1), 2:+(-1.5)), ((1,(-1)),2:+1)])
  -- let tr = transform 3 (pm 2 [((1,0),1.5:+1), ((3,1), 2:+(-1.5)), ((1,(-1)),2:+1)])
  -- let tr = transform 3 (pgg 2 [((1,0),1.5:+1), ((3,1), 2:+(-1.5)), ((1,(-1)),2:+1)])
  let tr = transform 1 (p4 [((1,-1), 1 :+ 0)])
  writePng "output.png" $ case dImg of
    Left msg -> error msg
    Right (ImageRGB8  img) -> tr (promoteImage img)
    Right (ImageRGBA8 img) -> tr img
    Right _                -> error "png images only please."
