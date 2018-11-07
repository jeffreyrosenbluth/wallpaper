---------------------------------------------------------------------------------
-- |
-- Module      :  Recipes.Frieze
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Recipes for the 17 wallpaper groups.
--
-- For more detailed descriptions of the various symmetry groups see:
-- https://en.wikipedia.org/wiki/Frieze_group
-- and
-- "Creating Symmetry" by Frank A. Farris, 2015 Princeton University Press,
--  Chapter 8
--
-- The color wheel used for all of the images:
--
-- <<examples/rose_small.png>>
---------------------------------------------------------------------------------

module Recipes.Frieze
  (
  -- * Frieze Groups
  -- ** Frieze functions
    p111
  , p211
  , p1m1
  , p11m
  , p11g
  , p2mm
  , p2mg

  -- ** Wave functions
  , nm
  ) where

import           Complextra
import           Core
import           Types

import           Data.List    (nub)

-- Wave functions ---------------------------------------------------------------

-- | Frieze wave function.
nm :: Int -> Int -> Recipe
nm n m z = exp (fromIntegral n .*^ (i * z))
         * exp (fromIntegral m .*^ (i * conjugate z))
  where
    i = 0 :+ 1

--  Frieze Recipes ---------------------------------------------------------------

-- | Translations only.
--
-- <<examples/p111.png>>
p111 :: [Coef] -> Recipe
p111  = mkRecipe nm

-- | 180 degree rotations and translations.
--
-- <<examples/p211.png>>
p211 :: [Coef] -> Recipe
p211 cs = mkRecipe nm (nub $ cs ++ (negateCoefs <$> cs))

-- | Vertical reflection and translations.
--
-- <<examples/p1m1.png>>
p1m1 :: [Coef] -> Recipe
p1m1 cs = mkRecipe nm (nub $ cs ++ (reverseCoefs <$> cs))

-- | Horizontal reflection and translations.
--
-- <<examples/p11m.png>>
p11m :: [Coef] -> Recipe
p11m cs = mkRecipe nm (nub $ cs ++ (negateCoefs . reverseCoefs <$> cs))

-- | Glide reflection and translations.
--
-- <<examples/p11g.png>>
p11g :: [Coef] -> Recipe
p11g cs = mkRecipe nm (nub $ cs ++ cs')
  where
    cs' = negateCoefs . reverseCoefs . alternateCoefs (\n m -> (-1) ^^ (n+m))<$> cs

-- | Horizontal and vertical reflections and translations.
--
-- <<examples/p2mm.png>>
p2mm :: [Coef] -> Recipe
p2mm cs = mkRecipe nm (nub $ cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs <$> cs
    cs2 = reverseCoefs <$> cs
    cs3 = negateCoefs <$> cs2

-- | Horizontal glide reflection and 180 rotation.
--
-- <<examples/p2mg.png>>
p2mg :: [Coef] -> Recipe
p2mg cs = mkRecipe nm (nub $ cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs . reverseCoefs . alternateCoefs (\n m -> (-1) ^^ (n+m)) <$> cs
    cs2 = negateCoefs <$> cs
    cs3 = reverseCoefs . alternateCoefs (\n m -> (-1) ^^ (n+m)) <$> cs
