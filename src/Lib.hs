module Lib where

import           Types
import           Complextra

import           Codec.Picture
import           Data.Complex

wallpaper :: RealFloat a => (Int -> Int -> Recipe a) -> [Coef a] -> Recipe a
wallpaper mkRecipe cs z = sum $ zipWith (*) as rs
  where
    as = anm <$> cs
    rs = ($ z) . uncurry mkRecipe <$> [(nCoord c, mCoord c) | c <- cs]

negateCnm :: Coef a -> Coef a
negateCnm (Coef n m a) = Coef (-n) (-m) a

negateCm :: Coef a -> Coef a
negateCm (Coef n m a) = Coef n (-m) a

reverseCnm :: Coef a -> Coef a
reverseCnm (Coef n m a) = Coef m n a

alternateCanm :: RealFloat a => (Int -> Int -> Int) -> Coef a -> Coef a
alternateCanm alt (Coef n m a) = Coef n m (fromIntegral (alt n m) .*^ a)

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

getColor :: RealFloat a => Image PixelRGBA8 -> Int -> Int -> a -> a
         -> Recipe a -> Int -> Int -> PixelRGBA8
getColor img wide high s t rcp i j = clamp i' j'
  where
    (x :+ y)       = (t/s) .*^ rcp ( (fromIntegral i - wide2) / t
                                  :+ (fromIntegral j - high2) / t )
    (i', j')       = (round x + w `div` 2, round y + h `div` 2)
    clamp m n      = if m < 0 || n < 0 || m >= w || n >= h
                       then PixelRGBA8 0 0 0 255 -- opaque black
                       else pixelAt img m n
    (wide2, high2) = (fromIntegral wide / 2, fromIntegral high /2)
    (w, h)         = (imageWidth img, imageHeight img)

transform :: RealFloat a => a -> a -> Int -> Int -> Recipe a -> Image PixelRGBA8
          -> Image PixelRGBA8
transform s t w h rcp img = generateImage (getPixel img s t rcp) w h

transform' :: RealFloat a => a -> a -> Int -> Int -> Recipe a -> Image PixelRGBA8
           -> Image PixelRGBA8
transform' s t w h rcp img = generateImage (getColor img w h s t rcp) w h
