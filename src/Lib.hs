{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Lib where

import           Recipes
import           Types
import           Complextra

import           Data.Complex

getPixel :: (RealFloat a, Img i, BlackWhite (Pxl i))
          => Recipe a -> Options a -> i -> Int -> Int -> Pxl i
getPixel rcp opts img i j = clamp (round x + w1 `div` 2) (round y + h1 `div` 2)
  where
    t        = focus opts
    (w1, h1) = (imgWidth img, imgHeight img)
    (x :+ y) = (scale opts * 0.5 * fromIntegral (min w1 h1))
            .*^ focusIn (width opts)
                        (height opts)
                        rcp
                        (fromIntegral i / t :+ fromIntegral j / t)
    clamp m n
      | m < 0 || n < 0 || m >= w1 || n >= h1 = black
      | otherwise = getPxl img m n

transform :: (RealFloat a, Img i, BlackWhite (Pxl i))
          => Recipe a -> Options a -> i -> i
transform rcp opts img = generateImg (getPixel rcp opts img) (width opts) (height opts)

blend :: (RealFloat a, Img i, BlackWhite (Pxl i))
      => Options a -> Recipe a -> Recipe a -> i-> i
blend opts rcp1 rcp2 = transform rcp opts
  where
    rcp z@(x :+ _) = let a = (x + m) / (2 * m)
                     in  a .*^ rcp2 z + (1 - a) .*^ rcp1 z
    m = max 1 (fromIntegral (width opts) / fromIntegral (height opts))

morph :: (RealFloat a, Img i, BlackWhite (Pxl i))
      => Recipe a -> Options a -> a -> i -> i
morph rcp opts c = transform rcp' opts
  where
    rcp' z@(x :+ _) = exp (pi * phi c ((x+1)/m) .*^ im) * rcp z 
    m = max 1 (fromIntegral (width opts) / fromIntegral (height opts))
    phi cut u
      | u < cut = 1
      | u > 1 - cut = -1
      | otherwise = (-2 / (1 - 2 * cut)) * (u - 0.5)
