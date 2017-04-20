{-# LANGUAGE FlexibleContexts      #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Core
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Tools for creating domain colorings
-------------------------------------------------------------------------------

module Core
  ( getColor
  , symmetry
  , blend
  , morph
  , mkRecipe
  , negateCoefs
  , negateFst
  , negateSnd
  , reverseCoefs
  , alternateCoefs
  ) where

import           Types
import           Complextra

import           Data.Complex

-- | Creates a function to get the color of pixel (i, j) from a color wheel (iamge)
--   given 'Options', a 'Recipe' and the color wheel.
getColor :: (RealFloat a, Img i, BlackWhite (Pxl i))
          => Options a -> Recipe a -> i -> Int -> Int -> Pxl i
getColor opts rcp wheel i j = clamp (round x + w1 `div` 2) (round y + h1 `div` 2)
  where
    (w1, h1) = (imgWidth wheel, imgHeight wheel)
    (x :+ y) = (scale opts * 0.5 * fromIntegral (min w1 h1))
            .*^ focusIn (width opts)
                        (height opts)
                        (repLength opts)
                        rcp
                        (fromIntegral i :+ fromIntegral j)
    clamp m n
      | m < 0 || n < 0 || m >= w1 || n >= h1 = black
      | otherwise = getPxl wheel m n

-- | Center the coordinates at the origin and scale them to the range (-1, 1)
focusIn :: RealFloat a => Int -> Int -> Int -> Recipe a -> Recipe a
focusIn w h l rcp (x :+ y) =
  rcp ((x - fromIntegral w / 2) / l' :+ (fromIntegral h / 2 - y) / l')
    where
      l' = fromIntegral l

-- | Make a symmetry image from a set of 'Options', a 'Recipe' and a color wheel.
symmetry :: (RealFloat a, Img i, BlackWhite (Pxl i))
          => Options a -> Recipe a -> i -> i
symmetry opts rcp wheel = generateImg (getColor opts rcp wheel) (width opts) (height opts)

-- | Make a symmetry image from two 'Recipe's by linearly interpolation.
--   The interpolation is along the horizontal axis.
blend :: (RealFloat a, Img i, BlackWhite (Pxl i))
      => Options a -> Recipe a -> Recipe a -> i-> i
blend opts rcp1 rcp2 = symmetry opts rcp
  where
    rcp z@(x :+ _) = let a = (x + m) / (2 * m)
                     in  a .*^ rcp2 z + (1 - a) .*^ rcp1 z
    m = max 1 (fromIntegral (width opts) / fromIntegral (height opts))

-- | Make a symmetry image by interpolating between a color wheel and its 180
--   degree rotation. The cutoff represents what percentage of the
--   image stays constant at the left and right sides. Like 'blend' the
--   interpolation is in the horizontal direction.
morph :: (RealFloat a, Img i, BlackWhite (Pxl i))
      => Options a -> Recipe a -> a -> i -> i
morph opts rcp c = symmetry opts rcp'
  where
    rcp' z@(x :+ _) = exp (pi * phi c ((x+t/2)/t) .*^ im) * rcp z
    t = fromIntegral (width opts `div`repLength opts)
    phi cut u
      | u < cut = 1
      | u > 1 - cut = -1
      | otherwise = (2 / (2 * cut - 1)) * (u - 0.5)

-- | Make a recipe from a lattice and a list of Coefficients.
mkRecipe :: RealFloat a => (Int -> Int -> Recipe a) -> [Coef a] -> Recipe a
mkRecipe rf cs z = sum $ zipWith (*) as rs
  where
    as = anm <$> cs
    rs = ($ z) . uncurry rf <$> [(nCoord c, mCoord c) | c <- cs]

-- | Negate the indices of a coefficient.
negateCoefs :: Coef a -> Coef a
negateCoefs (Coef n m a) = Coef (-n) (-m) a

-- | Negate the first index of a coefficient.
negateFst :: Coef a -> Coef a
negateFst (Coef n m a) = Coef (-n) m a

-- | Negate the second index of a coefficient.
negateSnd :: Coef a -> Coef a
negateSnd (Coef n m a) = Coef n (-m) a

-- | Reverse the indices of a coefficient.
reverseCoefs :: Coef a -> Coef a
reverseCoefs (Coef n m a) = Coef m n a

-- | Multiply a coefficient by a function of its indices, usually used
--   to change the sign of a coefficient based on its indices.
--   Does not commute with negate or reverse, usually you want to apply
--   'alternateCoefs' first.
alternateCoefs :: RealFloat a => (Int -> Int -> Int) -> Coef a -> Coef a
alternateCoefs alt (Coef n m a) = Coef n m (fromIntegral (alt n m) .*^ a)
