module Types where

import Data.Complex

type Recipe a = Complex a -> Complex a

data Coef a = Coef
  { nCoord :: Int
  , mCoord :: Int
  , anm    :: Complex a
  }
