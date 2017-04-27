module Recipes.Rosette where

import           Data.Complex
import           Types

recipe5 :: RealFloat a => Complex a -> Complex a -> Recipe a
recipe5 a b z = z**5 + z'**5
              + a * (z**6 * z' + z * z'**6)
              + b * (z**4 * z'**(-6) + z**(-6) * z'**4)
  where
    z' = conjugate z
