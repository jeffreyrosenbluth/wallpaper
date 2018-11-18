{-# LANGUAGE FlexibleContexts    #-}
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
import           Data.List     (foldl')

-- | Specialized alias for fromIntegral.
dbl :: Int -> Double
dbl = fromIntegral

-- Domain Coloring -------------------------------------------------------------
-- | Creates a function to get the color of pixel (i, j) from a color wheel
--   given 'Options', a 'Recipe' and the color wheel.
color :: Pixel p => Options -> Recipe -> Image p -> Int -> Int -> p
color opts rcp wheel i j = pixel (round $ re z + dbl w / 2 + x)
                                 (round $ im z + dbl h / 2 + y)
  where
    (w, h)     = (imageWidth wheel, imageHeight wheel)
    (x, y)     = (fst (origin opts) * dbl w / 2, - snd (origin opts) * dbl h / 2)
    r          = min (dbl w / 2 - abs x) (dbl h / 2 - abs y)
    f          = focusIn opts rcp (ints2Complex i j)
    z          = f * fromDouble (scale opts * r) * cis (rotation opts)
    pixel m n  = pixelAt wheel (clamp w m) (clamp h n)

-- | Helper function to restrict an number to [0, w-1].
clamp :: (Num a, Ord a) => a -> a -> a
clamp w x
  | x < 0     = 0
  | x >= w    = w - 1
  | otherwise = x

-- | Center the coordinates at the origin and scale them based on 'repLength'
focusIn :: Options -> Recipe -> Recipe
focusIn opts rcp (x :+ y) = rcp ((x - w / 2) / replen :+ (h / 2 - y) / replen)
  where
    [replen, w, h] = dbl <$> [repLength opts, width opts, height opts]

-- | Make a recipe from a lattice and a list of Coefficients.
mkRecipe :: (Int -> Int -> Recipe) -> [Coef] -> Recipe
mkRecipe rf cs = (* fromDouble (1/s)) . f
  where
    f x = sum $ (\coef -> anm coef * rf (nCoord coef) (mCoord coef) x) <$> cs
    m   = foldl' (\a b -> max a (magnitude . f $ cis (pi * b / 180))) 0 [0..359]
    s   = if m > 0 then m else 1

-- | Make an image from a set of 'Options', a 'Recipe' and a color source.
domainColoring :: (Pixel p) => Options -> Recipe -> ColorSource p -> Image p
domainColoring opts rcp source = generateImage colorOf (width opts) (height opts)
  where
    colorOf i j = case source of
      Picture img -> color opts rcp img i j
      Function f  -> let rcp' = focusIn opts rcp
                     in  f . rcp' $ ints2Complex i j

-- | Make a symmetry image from two 'Recipe's by linearly interpolation.
--   The interpolation is along the horizontal axis.
blend :: (Pixel p) => Options -> Recipe -> Recipe -> ColorSource p -> Image p
blend opts rcp1 rcp2 = domainColoring opts rcp
  where
    rcp z = let a = (re z + m) / (2 * m)
            in  fromDouble a * rcp2 z + fromDouble (1 - a) * rcp1 z
    m     = max 1 (dbl (width opts) / dbl (height opts))

-- | Make a symmetry image by interpolating between a color wheel and its 180
--   degree rotation. The cutoff represents what percentage of the
--   image stays constant at the left and right sides. Like 'blend' the
--   interpolation is in the horizontal direction.
morph :: (Pixel p) => Options -> Recipe -> Double -> ColorSource p -> Image p
morph opts rcp c = domainColoring opts rcp'
  where
    rcp' z@(x :+ _) = cis (pi * phi c ((x+t/2)/t)) * rcp z
    t = dbl (width opts `div`repLength opts)
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
