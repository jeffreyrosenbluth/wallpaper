module Types where

import Data.Complex

type Recipe = Complex Double -> Complex Double

type Coefficent = ((Int, Int), Complex Double)

data Coeff = Coeff
  { nCoord :: Int
  , mCoord :: Int
  , anm    :: Complex Double
  }
