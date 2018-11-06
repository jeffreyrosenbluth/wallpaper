--------------------------------------------------------------------------------
-- |
-- Module      :  Complextra
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Utility functions for dealing with complex numbers.
--------------------------------------------------------------------------------

module Complextra
  ( scaleZ
  , (.*^)
  , fromReal
  , im
  , re
  ) where

import           Data.Complex

-- | Multiply a complex number by a real number.
scaleZ :: RealFloat a => a -> Complex a -> Complex a
scaleZ k z = (k :+ 0) * z

-- | Infix form of 'scaleZ'.
(.*^) :: RealFloat a => a -> Complex a -> Complex a
(.*^) = scaleZ
infixl 7 .*^

fromReal :: Num a => a -> Complex a
fromReal x = x :+ 0

im :: Complex a -> a
im = imagPart

re :: Complex a -> a
re = realPart
