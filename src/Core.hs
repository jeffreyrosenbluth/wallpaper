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
import           Data.List (foldl')

-- Domain Coloring -------------------------------------------------------------
-- | Creates a function to get the color of pixel (i, j) from a color wheel
--   given 'Options', a 'Recipe' and the color wheel. You shouldn't need to
--   use this function directly.
getColor :: (Pixel p, BlackWhite p)
          => Options -> Recipe -> Image p -> Int -> Int -> p
getColor opts rcp wheel i j = color (round (re z) + w `div` 2 + round x)
                                    (round (im z) + h `div` 2 + round y)
  where
    (x', y') = origin opts
    (w, h)   = (imageWidth wheel, imageHeight wheel)
    (w', h')   = (fromIntegral w, fromIntegral h)
    (x, y)     = (x' * w' / 2, -y' * h' / 2)
    r          = min (w' / 2 - abs x) (h' / 2 - abs y)
    f          = focusIn (width opts)
                         (height opts)
                         (repLength opts)
                         rcp
                         (fromIntegral i :+ fromIntegral j)
    z          = f * fromDouble (scale opts * r) * cis (rotation opts)
    color m n  = pixelAt wheel (clamp w m) (clamp h n)

clamp :: Int -> Int -> Int
clamp w x
  | x < 0  = 0
  | x >= w = w - 1
  | otherwise = x

-- | Center the coordinates at the origin and scale them based on 'repLength'
focusIn :: Int -> Int -> Int -> Recipe -> Recipe
focusIn w h l rcp (x :+ y) =
  rcp ((x - fromIntegral w / 2) / l' :+ (fromIntegral h / 2 - y) / l')
    where
      l' = fromIntegral l

-- | Make a recipe from a lattice and a list of Coefficients.
mkRecipe :: (Int -> Int -> Recipe) -> [Coef] -> Recipe
mkRecipe rf cs = (* fromDouble (1/s)) . f
  where
    degrees = fromIntegral <$> [0..359 :: Int]
    f x = sum ((\(i, j, y) -> y * rf i j x)
      <$> [(nCoord c, mCoord c, anm c) | c <- cs])
    m  = foldl' (\a b -> max a (magnitude . f $ cis (pi * b / 180))) 0 degrees
    s  = if m > 0 then m else 1

-- | Make an image from a set of 'Options', a 'Recipe' and a color source.
domainColoring :: (Pixel p, BlackWhite p)
          => Options -> Recipe -> ColorSource p -> Image p
domainColoring opts rcp source = generateImage color (width opts) (height opts)
  where
    color i j = case source of
      Picture img -> getColor opts rcp img i j
      Function f  ->
        let rcp' = focusIn (width opts) (height opts) (repLength opts) rcp
        in  f . rcp' $ (fromIntegral i :+ fromIntegral j)

-- | Make a symmetry image from two 'Recipe's by linearly interpolation.
--   The interpolation is along the horizontal axis.
blend :: (Pixel p, BlackWhite p)
      => Options -> Recipe -> Recipe -> ColorSource p -> Image p
blend opts rcp1 rcp2 = domainColoring opts rcp
  where
    rcp z = let a = (re z + m) / (2 * m)
            in  fromDouble a * rcp2 z + fromDouble (1 - a) * rcp1 z
    m     = max 1 (fromIntegral (width opts) / fromIntegral (height opts))

-- | Make a symmetry image by interpolating between a color wheel and its 180
--   degree rotation. The cutoff represents what percentage of the
--   image stays constant at the left and right sides. Like 'blend' the
--   interpolation is in the horizontal direction.
morph :: (Pixel p, BlackWhite p)
      => Options -> Recipe -> Double -> ColorSource p -> Image p
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
negateCoefs :: Coef -> Coef
negateCoefs (Coef n m a) = Coef (-n) (-m) a

-- | Negate the first index of a coefficient.
negateFst :: Coef -> Coef
negateFst (Coef n m a) = Coef (-n) m a

-- | Negate the second index of a coefficient.
negateSnd :: Coef -> Coef
negateSnd (Coef n m a) = Coef n (-m) a

-- | Reverse the indices of a coefficient.
reverseCoefs :: Coef-> Coef
reverseCoefs (Coef n m a) = Coef m n a

-- | Multiply a coefficient by a function of its indices, usually used
--   to change the sign of a coefficient based on its indices.
--   Does not commute with negate or reverse, usually you want to apply
--   'alternateCoefs' first.
alternateCoefs :: (Int -> Int -> Double) -> Coef -> Coef
alternateCoefs alt (Coef n m a) = Coef n m (fromDouble (alt n m) * a)
