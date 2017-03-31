module Complextra where

import Data.Complex

scaleZ :: RealFloat a => a -> Complex a -> Complex a
scaleZ k z = (k :+ 0) * z

(.*^) :: RealFloat a => a -> Complex a -> Complex a
(.*^) = scaleZ

infixl 7 .*^

im :: Num a => Complex a
im = 0 :+ 1
