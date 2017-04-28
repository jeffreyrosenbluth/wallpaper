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

import           Data.Complex
import           Data.List    (nub)

-- Wave functions ---------------------------------------------------------------

-- | Frieze wave function.
nm :: RealFloat a => Int -> Int -> Recipe a
nm n m z = exp (fromIntegral n .*^ (im * z))
         * exp (fromIntegral m .*^ (im * conjugate z))

--  Frieze Recipes ---------------------------------------------------------------

-- | Translations only.
p111 :: RealFloat a => [Coef a] -> Recipe a
p111  = mkRecipe nm

-- | 180 degree rotations and translations.
p211 :: RealFloat a => [Coef a] -> Recipe a
p211 cs = mkRecipe nm (nub $ cs ++ (negateCoefs <$> cs))

-- | Vertical reflection and translations.
p1m1 :: RealFloat a => [Coef a] -> Recipe a
p1m1 cs = mkRecipe nm (nub $ cs ++ (reverseCoefs <$> cs))

-- | Horizontal reflection and translations.
p11m :: RealFloat a => [Coef a] -> Recipe a
p11m cs = mkRecipe nm (nub $ cs ++ (negateCoefs . reverseCoefs <$> cs))

-- | Glide reflection and translations.
p11g :: RealFloat a => [Coef a] -> Recipe a
p11g cs = mkRecipe nm (nub $ cs ++ cs')
  where
    cs' = negateCoefs . reverseCoefs . alternateCoefs (\n m -> (-1) ^^ (n+m))<$> cs

-- | Horizontal and vertical reflections and translations.
p2mm :: RealFloat a => [Coef a] -> Recipe a
p2mm cs = mkRecipe nm (nub $ cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs <$> cs
    cs2 = reverseCoefs <$> cs
    cs3 = negateCoefs <$> cs2

-- | Horizontal glide reflection and 180 rotation.
p2mg :: RealFloat a => [Coef a] -> Recipe a
p2mg cs = mkRecipe nm (nub $ cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs . reverseCoefs . alternateCoefs (\n m -> (-1) ^^ (n+m)) <$> cs
    cs2 = negateCoefs <$> cs
    cs3 = reverseCoefs . alternateCoefs (\n m -> (-1) ^^ (n+m)) <$> cs
