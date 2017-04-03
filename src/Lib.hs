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

-- | Does not commute with negate or reverse, usuall you want to apply
--   'alternateCanm' first.
alternateCanm :: RealFloat a => (Int -> Int -> Int) -> Coef a -> Coef a
alternateCanm alt (Coef n m a) = Coef n m (fromIntegral (alt n m) .*^ a)

getPixel :: (RealFloat a, Pixel p, BlackWhite p) => Image p -> Int -> Int -> a -> a
         -> Recipe a -> Int -> Int -> p
getPixel img wide high s t rcp i j = clamp (round x + w `div` 2) (round y + h `div` 2)
  where
    (x :+ y)       = (s * 0.5 * fromIntegral w) .*^ rcp (i' / t' :+ j' / t')
    (wide2, high2) = (0.5 * fromIntegral wide, 0.5 * fromIntegral high)
    (i', j')       = (fromIntegral i - wide2, fromIntegral j - high2)
    (w, h)         = (imageWidth img, imageHeight img)
    t'             = t * min wide2 high2 
    clamp m n
      | m < 0 || n < 0 || m >= w || n >= h = black
      | otherwise = pixelAt img m n

transform :: RealFloat a => a -> a -> Int -> Int -> Recipe a -> Image PixelRGBA8
          -> Image PixelRGBA8
transform s t w h rcp img = generateImage (getPixel img w h s t rcp) w h

blend :: RealFloat a => a -> a -> Int -> Int -> Recipe a -> Recipe a -> Image PixelRGBA8 -> Image PixelRGBA8
blend s t w h rcp1 rcp2 = transform s t w h rcp 
  where
    rcp z@(x :+ y) = let a = (x + m) / (2 * m)
                     in  a .*^ rcp2 z + (1 - a) .*^ rcp1 z
    m = max 1 (fromIntegral w / fromIntegral h)
    clamp a
      | a < 0.2    = 0
      | a > 0.8    = 1
      | otherwise  = a
