---------------------------------------------------------------------------------
-- |
-- Module      :  Recipes.Wallpaper
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Recipes for the 17 wallpaper groups.
--
-- For more detailed descriptions of the various symmetry groups see:
-- https://en.wikipedia.org/wiki/Wallpaper_group
-- and
-- "Creating Symmetry" by Frank A. Farris, 2015 Princeton University Press,
--  Appendices A and B
--
-- The color wheel used for all of the images:
--
-- <<examples/rose_small.png>>
--
-- placed side by side with it's negative, that's where the purples come from.
---------------------------------------------------------------------------------

module Recipes.Wallpaper
  (
   -- * Wallpaper Groups

   -- ** Generic Lattice
   -- | Lattice vectors: __1, xi + i * eta__.
   genericLattice
  , p1
  , p2

   -- ** Rhombic (Centered) Lattice
   -- | Lattice vectors: __1//2 + i * b, 1//2 - i * b__.
  , rhombicLattice
  , cm
  , cmm

   -- ** Rectangular Lattice
   -- | Lattice vectors: __1, i * l__.
  , rectangularLattice
  , pm
  , pg
  , pmm
  , pmg
  , pgg

   -- ** Square Lattice
   -- | Lattice vectors: __1, i__.
  , squareLattice
  , p4
  , p4m
  , p4g

   -- ** Hexagonal Lattice
   -- | Lattice vectors: __1, (-1 + i * sqrt(3)) // 2__.
  , hexagonalLattice
  , p3
  , p31m
  , p3m1
  , p6
  , p6m

   -- ** Wave Functions
  , enm
  , tnm
  , wnm
  ) where

import           Complextra
import           Core
import           Types

import           Data.Complex
import           Data.List    (nub)

-- Wave functions --------------------------------------------------------------

-- | Periodic waves with respect to two translations. A Fourier vector.
enm :: RealFloat a => Int -> Int -> a -> a -> Complex a
enm n m x y = exp (2 * pi * (fromIntegral n * x + fromIntegral m * y) .*^ im)

-- | Wave packets to create 2-fold rotational symmetry.
tnm :: RealFloat a => Int -> Int -> a -> a -> Complex a
tnm n m x y = 0.5 * (enm n m x y + enm (-n) (-m) x y)

-- | Wave packets to create 3-fold rotational symmetry.
wnm :: RealFloat a => Int -> Int -> a -> a -> Complex a
wnm n m x y = (1/3) * (enm n m x y + enm m (-n - m) x y + enm (-n - m) n x y)

-- Recipes for the Generic Lattice ---------------------------------------------
genericLattice :: RealFloat a => a -> a -> Int -> Int -> Recipe a
genericLattice xi eta n m (x :+ y) = enm n m x' y'
  where
    x' = x - xi * y / eta
    y' = y / eta

-- | The symmetry group with translations only.
--
-- <<examples/p1.png>>
p1 :: RealFloat a => a -> a -> [Coef a] -> Recipe a
p1 xi eta =  mkRecipe (genericLattice xi eta)

-- | The symmetry group with four rotational centers of order 2, 180 degree
--   rotational symmetry.
--
-- <<examples/p2.png>>
p2 :: RealFloat a => a -> a -> [Coef a] -> Recipe a
p2 xi eta cs = mkRecipe (genericLattice xi eta) (nub $cs ++ (negateCoefs <$> cs))

-- Rhombic Lattice -------------------------------------------------------------

-- | Rhombic Lattice for creating symmmetry about the center.
rhombicLattice :: RealFloat a => a -> Int -> Int -> Recipe a
rhombicLattice b n m (x :+ y) = enm n m x' y'
  where
    x' = x + y / (2*b)
    y' = x - y / (2*b)

-- | Reflection about the horizontal axis plus horizontal glide reflection.
--
-- <<examples/cm.png>>
cm :: RealFloat a => a -> [Coef a] -> Recipe a
cm b cs = mkRecipe (rhombicLattice b) (nub $ cs ++ (reverseCoefs <$> cs))

-- | Rotaion and Reflection about the horizontal axis in addition to translation
--   invariance about the center of the lattice.
--
-- <<examples/cmm.png>>
cmm :: RealFloat a => a -> [Coef a] -> Recipe a
cmm b cs = mkRecipe (rhombicLattice b) (nub $ cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs <$> cs
    cs2 = reverseCoefs <$> cs
    cs3 = reverseCoefs . negateCoefs <$> cs

-- Rectangular Lattice ---------------------------------------------------------

-- | Rectangular Lattice for creating symmetry with no rotational symmetry.
rectangularLattice :: RealFloat a => a -> Int -> Int -> Recipe a
rectangularLattice l n m (x :+ y) = enm n m x (y / l)

-- | Rectangular Lattice for creating symmetry with 2-fold rotational symmetry.
rectangularLattice2 :: RealFloat a => a -> Int -> Int -> Recipe a
rectangularLattice2 l n m (x :+ y) = tnm n m x (y / l)

-- | Reflection about the horizontal axis.
--
-- <<examples/pm.png>>
pm :: RealFloat a => a -> [Coef a] -> Recipe a
pm l cs = mkRecipe (rectangularLattice l) (nub $ cs ++ (negateSnd <$> cs))

-- | Glide reflection in the horizontal direction.
--
-- <<examples/pg.png>>
pg :: RealFloat a => a -> [Coef a] -> Recipe a
pg l cs = mkRecipe (rectangularLattice l) (nub $ cs ++ cs')
  where
    cs' = negateSnd . alternateCoefs (\n _ -> (-1) ^^ n) <$> cs

-- | Reflection about the horizontal and vertical axis
--   in addition to 2-fold symmetry.
--
-- <<examples/pmm.png>>
pmm :: RealFloat a => a -> [Coef a] -> Recipe a
pmm l cs = mkRecipe (rectangularLattice2 l) (nub $ cs ++ (negateSnd <$> cs))

-- | Glide Reflection about the horizontal axis in addition to 2-fold symmetry.
--
-- <<examples/pmg.png>>
pmg :: RealFloat a => a -> [Coef a] -> Recipe a
pmg l cs = mkRecipe (rectangularLattice2 l) (nub $ cs ++  cs')
  where
    cs' = negateSnd . alternateCoefs (\n _ -> (-1) ^^ n) <$> cs

-- | Glide Reflection about the line x=1/4 in addition to 2-fold symmetry.
--
-- <<examples/pgg.png>>
pgg :: RealFloat a => a -> [Coef a] -> Recipe a
pgg l cs = mkRecipe (rectangularLattice2 l) (nub $ cs ++ cs')
  where
    cs' = negateSnd . alternateCoefs (\n m -> (-1) ^^ (n+m)) <$> cs

-- Square Latticd---------------------------------------------------------------

-- | Square Lattice for creating 4-fold symmetry.
squareLattice :: RealFloat a => Int -> Int -> Recipe a
squareLattice n m (x :+ y) = 0.5 * (tnm n m x y + tnm (-n) m x y)

-- | 4-fold symmetry only.
--
-- <<examples/p4.png>>
p4 :: RealFloat a => [Coef a] -> Recipe a
p4 = mkRecipe squareLattice

-- | Reflection along the diagonal of the square in addition to 4-fold symmetry.
--
-- <<examples/p4m.png>>
p4m :: RealFloat a => [Coef a] -> Recipe a
p4m cs = mkRecipe squareLattice (nub $ cs ++ (reverseCoefs <$> cs))

-- | Glide symmetry about the diagonal of the sqaure in addition to
--   4-fold symmetry.
--
-- <<examples/p4g.png>>
p4g :: RealFloat a => [Coef a] -> Recipe a
p4g cs = mkRecipe squareLattice (nub $ cs ++ cs')
  where
    cs' = reverseCoefs . alternateCoefs (\n m -> (-1) ^^ (n+m)) <$> cs

-- Hexagonal Lattice -----------------------------------------------------------

-- | Hexagonal Lattice for creating 3-fold symmetry.
hexagonalLattice :: RealFloat a => Int -> Int -> Recipe a
hexagonalLattice n m (x :+ y) = (1/3) * (enm n m x' y' + enm m (-n - m) x' y' + enm (-n - m) n x' y')
  where
    x' = x + y / sqrt3
    y' = 2 * y / sqrt3
    sqrt3 = sqrt 3

-- | 3-fold symmetry only.
--
-- <<examples/p3.png>>
p3 :: RealFloat a => [Coef a] -> Recipe a
p3 = mkRecipe hexagonalLattice

-- | Reflection about the horizontal axis in addition to 3-fold symmetry.
--
-- <<examples/p31m.png>>
p31m :: RealFloat a => [Coef a] -> Recipe a
p31m cs = mkRecipe hexagonalLattice (nub $ cs ++ (reverseCoefs <$> cs))

-- | Reflction about the vertical axis in addtion to 3-fold symmetry.
--
-- <<examples/p3m1.png>>
p3m1 :: RealFloat a => [Coef a] -> Recipe a
p3m1 cs = mkRecipe hexagonalLattice (nub $ cs ++ (negateCoefs . reverseCoefs <$> cs))

-- | 60 degree Rotation in addtion to 3-fold symmetry.
--
-- <<examples/p6.png>>
p6 :: RealFloat a => [Coef a] -> Recipe a
p6 cs = mkRecipe hexagonalLattice (nub $ cs ++ (negateCoefs <$> cs))

-- | 60 degree Rotation and reflection about the horizontal in addtion
--   to 3-fold symmetry.
--
-- <<examples/p6m.png>>
p6m :: RealFloat a => [Coef a] -> Recipe a
p6m cs = mkRecipe hexagonalLattice (nub $ cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs <$> cs
    cs2 = reverseCoefs <$> cs
    cs3 = negateCoefs <$> cs2
