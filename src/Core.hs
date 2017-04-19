{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Core where

import           Recipe
import           Types
import           Complextra

import           Data.Complex

getColor :: (RealFloat a, Img i, BlackWhite (Pxl i))
          => Options a -> Recipe a -> i -> Int -> Int -> Pxl i
getColor opts rcp img i j = clamp (round x + w1 `div` 2) (round y + h1 `div` 2)
  where
    (w1, h1) = (imgWidth img, imgHeight img)
    (x :+ y) = (scale opts * 0.5 * fromIntegral (min w1 h1))
            .*^ focusIn (width opts)
                        (height opts)
                        (repLength opts)
                        rcp
                        (fromIntegral i :+ fromIntegral j)
    clamp m n
      | m < 0 || n < 0 || m >= w1 || n >= h1 = black
      | otherwise = getPxl img m n

transform :: (RealFloat a, Img i, BlackWhite (Pxl i))
          => Options a -> Recipe a -> i -> i
transform opts rcp img = generateImg (getColor opts rcp img) (width opts) (height opts)

blend :: (RealFloat a, Img i, BlackWhite (Pxl i))
      => Options a -> Recipe a -> Recipe a -> i-> i
blend opts rcp1 rcp2 = transform opts rcp
  where
    rcp z@(x :+ _) = let a = (x + m) / (2 * m)
                     in  a .*^ rcp2 z + (1 - a) .*^ rcp1 z
    m = max 1 (fromIntegral (width opts) / fromIntegral (height opts))

morph :: (RealFloat a, Img i, BlackWhite (Pxl i))
      => Options a -> Recipe a -> a -> i -> i
morph opts rcp c = transform opts rcp'
  where
    rcp' z@(x :+ _) = exp (pi * phi c ((x+t/2)/t) .*^ im) * rcp z
    t = fromIntegral (width opts `div`repLength opts)
    phi cut u
      | u < cut = 1
      | u > 1 - cut = -1
      | otherwise = (2 / (2 * cut - 1)) * (u - 0.5)
