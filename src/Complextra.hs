--------------------------------------------------------------------------------
-- |
-- Module      :  Complexlextra
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Utility functions for dealing with Complexlex numbers.
--------------------------------------------------------------------------------

module Complextra
  ( Complex((:+))
  , cis
  , magnitude
  , phase
  , conjugate
  , mkPolar
  , polar
  , scaleZ
  , (.*^)
  , fromReal
  , im
  , re
  ) where

data Complex = !Double :+ !Double
  deriving (Eq, Show, Read)

infix  6  :+

-- | Multiply a Complexlex number by a real number.
scaleZ :: Double -> Complex -> Complex
scaleZ k z = (k :+ 0) * z

-- | Infix form of 'scaleZ'.
(.*^) :: Double -> Complex -> Complex
(.*^) = scaleZ
infixl 7 .*^

fromReal :: Double -> Complex
fromReal x = x :+ 0

im :: Complex -> Double
im (_ :+ b)= b

re :: Complex -> Double
re (a :+ _) = a

-- | The conjugate of a complex number.
conjugate :: Complex -> Complex
conjugate (x:+y) = x :+ (-y)

-- | Form a complex number from polar components of magnitude and phase.
mkPolar :: Double -> Double -> Complex
mkPolar r theta = r * cos theta :+ r * sin theta

-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair in canonical form:
-- the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
-- if the magnitude is zero, then so is the phase.
polar :: Complex -> (Double, Double)
polar z = (magnitude z, phase z)

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
cis :: Double -> Complex
cis theta = cos theta :+ sin theta

-- | The nonnegative magnitude of a complex number.
magnitude :: Complex -> Double
magnitude (x:+y) = sqrt (x * x + y * y)

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
phase :: Complex -> Double
phase (0 :+ 0) = 0
phase (x:+y)   = atan2 y x

instance Num Complex where
    (x:+y) + (x':+y') =  (x+x') :+ (y+y')
    (x:+y) - (x':+y') =  (x-x') :+ (y-y')
    (x:+y) * (x':+y') =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)     =  negate x :+ negate y
    abs z             =  magnitude z :+ 0
    signum (0:+0)     =  0
    signum z@(x:+y)   =  x/r :+ y/r  where r = magnitude z
    fromInteger n     =  fromInteger n :+ 0

-- | @since 2.01
instance  Fractional Complex where
    (x:+y) / (x':+y')   =  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
                           where x'' = scaleFloat k x'
                                 y'' = scaleFloat k y'
                                 k   = - max (exponent x') (exponent y')
                                 d   = x'*x'' + y'*y''

    fromRational a      =  fromRational a :+ 0

-- | @since 2.01
instance  Floating Complex where
    pi             =  pi :+ 0
    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x
    log z          =  log (magnitude z) :+ phase z

    x ** y = case (x,y) of
      (_ , 0:+0)  -> 1 :+ 0
      (0:+0, exp_re:+_) -> case compare exp_re 0 of
                 GT -> 0 :+ 0
                 LT -> inf :+ 0
                 EQ -> nan :+ nan
      (re':+im', exp_re:+_)
        | isInfinite re' || isInfinite im' -> case compare exp_re 0 of
                 GT -> inf :+ 0
                 LT -> 0 :+ 0
                 EQ -> nan :+ nan
        | otherwise -> exp (log x * y)
      where
        inf = 1/0
        nan = 0/0

    sqrt (0:+0)    =  0
    sqrt z@(x:+y)  =  u :+ (if y < 0 then -v else v)
                      where (u,v) = if x < 0 then (v',u') else (u',v')
                            v'    = abs y / (u'*2)
                            u'    = sqrt ((magnitude z + abs x) / 2)

    sin (x:+y)     =  sin x * cosh y :+ cos x * sinh y
    cos (x:+y)     =  cos x * cosh y :+ (- sin x * sinh y)
    tan (x:+y)     =  (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
                      where sinx  = sin x
                            cosx  = cos x
                            sinhy = sinh y
                            coshy = cosh y

    sinh (x:+y)    =  cos y * sinh x :+ sin  y * cosh x
    cosh (x:+y)    =  cos y * cosh x :+ sin y * sinh x
    tanh (x:+y)    =  (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
                      where siny  = sin y
                            cosy  = cos y
                            sinhx = sinh x
                            coshx = cosh x

    asin z@(x:+y)  =  y':+(-x')
                      where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
    acos z         =  y'':+(-x'')
                      where (x'':+y'') = log (z + ((-y'):+x'))
                            (x':+y')   = sqrt (1 - z*z)
    atan z@(x:+y)  =  y':+(-x')
                      where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))

    asinh z        =  log (z + sqrt (1+z*z))
    acosh z        =  log (z + sqrt (z+1) * sqrt (z-1))
    atanh z        =  0.5 * log ((1.0+z) / (1.0-z))
