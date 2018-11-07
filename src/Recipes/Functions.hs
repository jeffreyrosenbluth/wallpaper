
---------------------------------------------------------------------------------
-- |
-- Module      :  Recipes.Functions
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Recipes for functions of a Complex variable.
--
-- The color wheel used for all of the images:
--
-- <<examples/rose_small.png>>
---------------------------------------------------------------------------------

module Recipes.Functions
  (
  -- * Functions
    identity
  , standard
  ) where

import           Types

identity :: Recipe
identity = id

standard :: Recipe
standard z = (z - 1) / (z**2 + z + 1)
