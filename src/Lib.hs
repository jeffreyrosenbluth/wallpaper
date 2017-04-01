module Lib where

import           Types
import           Complextra

import           Codec.Picture
import           Data.Complex

getPixel :: RealFloat a => Image PixelRGBA8 -> a -> a -> Recipe a -> Int -> Int
         -> PixelRGBA8
getPixel img s t rcp i j = clamp i' j'
  where
    (w, h) = (imageWidth img, imageHeight img)
    (w2, h2) = (fromIntegral w / 2, fromIntegral h / 2)
    (x :+ y) = (t/s) .*^ rcp ((fromIntegral i - w2) / t :+ (fromIntegral j - h2) / t)
    (i', j') = (round x + w `div` 2, round y + h `div` 2)
    clamp m n = if m < 0 || n < 0 || m >= w || n >= h
                  then PixelRGBA8 0 0 0 255 -- opaque black
                  else pixelAt img m n

transform :: RealFloat a => a -> a -> Int -> Int -> Recipe a -> Image PixelRGBA8
          -> Image PixelRGBA8
transform s t w h rcp img = generateImage (getPixel img s t rcp) w h

