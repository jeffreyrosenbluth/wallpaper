module Recipes.Frieze where

import           Types
import           Core
import           Complextra

import           Data.Complex

nm :: RealFloat a => Int -> Int -> Recipe a
nm n m z = exp (fromIntegral n .*^ (im * z))
         * exp (fromIntegral m .*^ (im * conjugate z))

p111 :: RealFloat a => [Coef a] -> Recipe a
p111  = mkRecipe nm

p211 :: RealFloat a => [Coef a] -> Recipe a
p211 cs = mkRecipe nm (cs ++ (negateCoefs <$> cs))


p1m1 :: RealFloat a => [Coef a] -> Recipe a
p1m1 cs = mkRecipe nm (cs ++ (reverseCoefs <$> cs))

p11m :: RealFloat a => [Coef a] -> Recipe a
p11m cs = mkRecipe nm (cs ++ (negateCoefs . reverseCoefs <$> cs))

p11g :: RealFloat a => [Coef a] -> Recipe a
p11g cs = mkRecipe nm (cs ++ cs')
  where
    cs' = negateCoefs . reverseCoefs . alternateCoefs (\n m -> (-1) ^ (n+m))<$> cs

p2mm :: RealFloat a => [Coef a] -> Recipe a
p2mm cs = mkRecipe nm (cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs <$> cs
    cs2 = reverseCoefs <$> cs
    cs3 = negateCoefs <$> cs2

p2mg :: RealFloat a => [Coef a] -> Recipe a
p2mg cs = mkRecipe nm (cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs . reverseCoefs . alternateCoefs (\n m -> (-1) ^ (n+m)) <$> cs
    cs2 = negateCoefs <$> cs
    cs3 = reverseCoefs . alternateCoefs (\n m -> (-1) ^ (n+m)) <$> cs
