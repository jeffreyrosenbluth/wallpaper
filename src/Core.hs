{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Core
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Tools for creating symmtery images using the domain coloring algortihm.
--
-- <<examples/Y.png>>
--------------------------------------------------------------------------------

module Core
  (
    -- * Domain Coloring
    domainColoring
  , blend
  , morph
  , mkRecipe

    -- * Coefficients
  , negateCoefs
  , negateFst
  , negateSnd
  , reverseCoefs
  , alternateCoefs
  ) where

import           Complextra
import           Types

import           Codec.Picture
import           Data.Complex
import           Data.List (foldl')

-- Domain Coloring -------------------------------------------------------------
-- | Creates a function to get the color of pixel (i, j) from a color wheel
--   given 'Options', a 'Recipe' and the color wheel. You shouldn't need to
--   use this function directly.
getColor :: (RealFloat a, Pixel p, BlackWhite p)
          => Options a -> Recipe a -> Image p -> Int -> Int -> p
getColor opts rcp wheel i j = clamp (floor (re z) + w1 `div` 2 + floor x)
                                    (floor (im z) + h1 `div` 2 + floor y)
  where
    (x0 :+ y0) = origin opts
    (w1, h1)   = (imageWidth wheel, imageHeight wheel)
    (w0, h0)   = (fromIntegral w1, fromIntegral h1)
    (x, y)     = (x0 * w0 / 2, -y0 * h0 / 2)
    r          = min (w0 / 2 - abs x) (h0 / 2 - abs y)
    f          = focusIn (width opts)
                         (height opts)
                         (repLength opts)
                         rcp
                         (fromIntegral i :+ fromIntegral j)
    z          = f * fromReal (scale opts * r) * cis (rotation opts)
    clamp m n  = pixelAt wheel m' n'
      where
        m' | m < 0 = 0
           | m >= w1 = w1 - 1
           | otherwise = m
        n' | n < 0 = 0
           | n >= h1 = h1 -1
           | otherwise = n

-- | Center the coordinates at the origin and scale them based on 'repLength'
focusIn :: RealFloat a => Int -> Int -> Int -> Recipe a -> Recipe a
focusIn w h l rcp (x :+ y) =
  rcp ((x - fromIntegral w / 2) / l' :+ (fromIntegral h / 2 - y) / l')
    where
      l' = fromIntegral l

-- | Make a recipe from a lattice and a list of Coefficients.
mkRecipe :: RealFloat a => (Int -> Int -> Recipe a) -> [Coef a] -> Recipe a
mkRecipe rf cs z = f z * fromReal (1/s)
  where
    degrees = fromIntegral <$> [0,59..359 :: Int]
    f x = sum ((\(i, j, y) -> y * rf i j x)
            <$> [(nCoord c, mCoord c, anm c) | c <- cs])
    m  = foldl' (\a b -> max a (magnitude . f $ cis (2 * pi * b / 360))) 0 degrees
    s  = if m > 0 then m else 1

-- | Make an image from a set of 'Options', a 'Recipe' and a color source.
domainColoring :: (RealFloat a, Pixel p, BlackWhite p)
          => Options a -> Recipe a -> ColorSource a p -> Image p
domainColoring opts rcp source = generateImage color (width opts) (height opts)
  where
    color i j = case source of
      Picture img -> getColor opts rcp img i j
      Function f  ->
        let rcp' = focusIn (width opts) (height opts) (repLength opts) rcp
        in  f . rcp' $ (fromIntegral i :+ fromIntegral j)

-- | Make a symmetry image from two 'Recipe's by linearly interpolation.
--   The interpolation is along the horizontal axis.
blend :: (RealFloat a, Pixel p, BlackWhite p)
      => Options a -> Recipe a -> Recipe a -> ColorSource a p -> Image p
blend opts rcp1 rcp2 = domainColoring opts rcp
  where
    rcp z = let a = (re z + m) / (2 * m)
            in  fromReal a * rcp2 z + fromReal (1 - a) * rcp1 z
    m     = max 1 (fromIntegral (width opts) / fromIntegral (height opts))

-- | Make a symmetry image by interpolating between a color wheel and its 180
--   degree rotation. The cutoff represents what percentage of the
--   image stays constant at the left and right sides. Like 'blend' the
--   interpolation is in the horizontal direction.
morph :: (RealFloat a, Pixel p, BlackWhite p)
      => Options a -> Recipe a -> a -> ColorSource a p -> Image p
morph opts rcp c = domainColoring opts rcp'
  where
    rcp' z@(x :+ _) = cis (pi * phi c ((x+t/2)/t)) * rcp z
    t = fromIntegral (width opts `div`repLength opts)
    phi cut u
      | u < cut = 1
      | u > 1 - cut = -1
      | otherwise = (2 / (2 * cut - 1)) * (u - 0.5)

-- Coefficients ----------------------------------------------------------------
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
alternateCoefs :: RealFloat a => (Int -> Int -> a) -> Coef a -> Coef a
alternateCoefs alt (Coef n m a) = Coef n m (fromReal (alt n m) * a)
