---------------------------------------------------------------------------------
-- |
-- Module      :  Recipes.Rosette
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Recipes for Rosettes.
--
-- For more detailed descriptions of rosettes as plane functions, seej
-- "Creating Symmetry" by Frank A. Farris, 2015 Princeton University Press,
--  Chapter 7
--
-- The color wheel used for all of the images:
--
-- <<examples/rose_small.png>>
---------------------------------------------------------------------------------

module Recipes.Rosette
  (
  -- * Rosettes
    rosetteP
  , rosettePM
  ) where

import           Core
import           Complextra
import           Data.List    (nub)
import           Types

entire :: Int -> Int -> Recipe
entire n m z = z ^^ n * conjugate z ^^ m

-- | Rosette recipe with p-fold symmetry.
--   /Note rosette recipe constuctors differ from those of wallpaper/
--  /and friezes in that they filter out coefficient coordinates that/
--  /do not satisfy n - m mod p = 0./
--
-- <<examples/rosetteP.png>>
rosetteP :: Int -> [Coef] -> Recipe
rosetteP p cs = mkRecipe entire cs'
  where
    cs' = filter (\(Coef n m _) -> ((n-m) `mod` p) == 0) cs

-- | Rosette recipe with p-fold and horizontal mirror symmetry.
--   /Note rosette recipe constuctors differ from those of wallpaper/
--  /and friezes in that they filter out coefficient coordinates that/
--  /do not satisfy n - m mod p = 0./
--
-- <<examples/rosettePM.png>>
rosettePM :: Int -> [Coef] -> Recipe
rosettePM p cs = mkRecipe entire (nub $ cs' ++ (reverseCoefs <$> cs'))
  where
    cs' = filter (\(Coef n m _) -> ((n-m) `mod` p) == 0) cs
