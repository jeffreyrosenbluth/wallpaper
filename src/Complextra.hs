--------------------------------------------------------------------------------
-- |
-- Module      :  Complextra
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Utility functions for dealing with complex numbers.
--------------------------------------------------------------------------------

module Complextra where

import           Data.Complex

-- | Multiply a complex number by a real number..
scaleZ :: RealFloat a => a -> Complex a -> Complex a
scaleZ k z = (k :+ 0) * z

-- | Infix form of 'scaleZ'.
(.*^) :: RealFloat a => a -> Complex a -> Complex a
(.*^) = scaleZ

infixl 7 .*^

-- | The square root of -1, i.
im :: Num a => Complex a
im = 0 :+ 1
