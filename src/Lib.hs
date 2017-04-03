module Lib where

import           Types
import           Complextra

import           Codec.Picture
import           Data.Complex

focusIn :: RealFloat a => Int -> Int -> Recipe a -> Recipe a
focusIn w h rcp (x :+ y) =
  rcp ((x - fromIntegral w / 2) / l :+ (y - fromIntegral h / 2) / l)
    where
      l = 0.5 * fromIntegral (min w h)

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
getPixel img w2 h2 s t rcp i j = clamp (round x + w1 `div` 2) (round y + h1 `div` 2)
  where
    (w1, h1) = (imageWidth img, imageHeight img)
    (x :+ y) = (s * 0.5 * fromIntegral (min w1 h1))
            .*^ focusIn w2 h2 rcp (fromIntegral i / t :+ fromIntegral j / t)
    clamp m n
      | m < 0 || n < 0 || m >= w1 || n >= h1 = black
      | otherwise = pixelAt img m n

transform :: (RealFloat a, Pixel p, BlackWhite p) => a -> a -> Int -> Int -> Recipe a -> Image p
          -> Image p
transform s t w h rcp img = generateImage (getPixel img w h s t rcp) w h

blend :: (RealFloat a, Pixel p, BlackWhite p)
      => a -> a -> Int -> Int -> Recipe a -> Recipe a -> Image p -> Image p
blend s t w h rcp1 rcp2 = transform s t w h rcp
  where
    rcp z@(x :+ _) = let a = (x + m) / (2 * m)
                     in  a .*^ rcp2 z + (1 - a) .*^ rcp1 z
    m = max 1 (fromIntegral w / fromIntegral h)

morph :: (RealFloat a, Pixel p, BlackWhite p)
      => a -> a -> a -> Int -> Int -> Recipe a -> Image PixelRGBA8 -> Image PixelRGBA8
morph s t c w h rcp = transform s t w h rcp'
  where
    rcp' z@(x :+ _) = exp (pi * phi c ((x+1)/m) .*^ im) * rcp z 
    m = max 1 (fromIntegral w / fromIntegral h)
    phi cut u
      | u < cut = 1
      | u > 1 - cut = -1
      | otherwise = (-2 / (1 - 2 * cut)) * (u - 0.5)
