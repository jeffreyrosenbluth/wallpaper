module Types where

import Data.Complex

type Recipe = Complex Double -> Complex Double

type Coefficent = ((Int, Int), Complex Double)
