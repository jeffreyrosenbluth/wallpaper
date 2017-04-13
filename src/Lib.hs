{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import           Recipes
import           Types
import           Complextra

import           Codec.Picture
import           Data.Complex

getPixel :: (RealFloat a, Pixel p, BlackWhite p)
         => Recipe a -> Options a -> Image p -> Int -> Int -> p
getPixel rcp opts img i j = clamp (round x + w1 `div` 2) (round y + h1 `div` 2)
  where
    t        = focus opts
    (w1, h1) = (imageWidth img, imageHeight img)
    (x :+ y) = (scale opts * 0.5 * fromIntegral (min w1 h1))
            .*^ focusIn (width opts)
                        (height opts)
                        rcp
                        (fromIntegral i / t :+ fromIntegral j / t)
    clamp m n
      | m < 0 || n < 0 || m >= w1 || n >= h1 = black
      | otherwise = pixelAt img m n

transform :: (RealFloat a, Pixel p, BlackWhite p) => Recipe a -> Options a -> Image p
          -> Image p
transform rcp opts img = generateImage (getPixel rcp opts img) (width opts) (height opts)

blend :: (RealFloat a, Pixel p, BlackWhite p)
      => Options a -> Recipe a -> Recipe a -> Image p -> Image p
blend opts rcp1 rcp2 = transform rcp opts
  where
    rcp z@(x :+ _) = let a = (x + m) / (2 * m)
                     in  a .*^ rcp2 z + (1 - a) .*^ rcp1 z
    m = max 1 (fromIntegral (width opts) / fromIntegral (height opts))

morph :: (RealFloat a, Pixel p, BlackWhite p)
      => Recipe a -> Options a -> a -> Image p -> Image p
morph rcp opts c = transform rcp' opts
  where
    rcp' z@(x :+ _) = exp (pi * phi c ((x+1)/m) .*^ im) * rcp z 
    m = max 1 (fromIntegral (width opts) / fromIntegral (height opts))
    phi cut u
      | u < cut = 1
      | u > 1 - cut = -1
      | otherwise = (-2 / (1 - 2 * cut)) * (u - 0.5)

jpResult :: Result (Image PixelRGBA8) Double PixelRGBA8
jpResult = Result transform
