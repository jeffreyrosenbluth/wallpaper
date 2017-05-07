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
import           Data.Complex
import           Data.List    (nub)
import           Types

entire :: RealFloat a => Int -> Int -> Recipe a
entire n m z = z ^^ n * conjugate z ^^ m

-- | Rosette recipe with p-fold symmetry.
--   /Note rosette recipe constuctors differ from those of wallpaper/
--  /and friezes in that they filter out coefficient coordinates that/
--  /do not satisfy n - m mod p = 0./
rosetteP :: RealFloat a => Int -> [Coef a] -> Recipe a
rosetteP p cs = mkRecipe entire cs'
  where
    cs' = filter (\(Coef n m _) -> ((n-m) `mod` p) == 0) cs

-- | Rosette recipe with p-fold and horizontal mirror symmetry.
--   /Note rosette recipe constuctors differ from those of wallpaper/
--  /and friezes in that they filter out coefficient coordinates that/
--  /do not satisfy n - m mod p = 0./
rosettePM :: RealFloat a => Int -> [Coef a] -> Recipe a
rosettePM p cs = mkRecipe entire (nub $ cs' ++ (reverseCoefs <$> cs'))
  where
    cs' = filter (\(Coef n m _) -> ((n-m) `mod` p) == 0) cs
