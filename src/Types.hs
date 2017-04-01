module Types where

import Data.Complex

type Recipe a = Complex a -> Complex a

data Coefficient a = Coefficient
  { nCoord :: Int
  , mCoord :: Int
  , anm    :: Complex a
  }
