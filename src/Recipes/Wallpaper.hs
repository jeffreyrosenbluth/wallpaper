module Recipes.Wallpaper where

import           Types
import           Core
import           Complextra

import           Data.Complex
import           Data.List     (nub)

mkCoef :: (Int, Int, Complex a) -> Coef a
mkCoef (n, m, a) = Coef n m a


enm :: RealFloat a => Int -> Int -> a -> a -> Complex a
enm n m x y = exp (2 * pi * (fromIntegral n * x + fromIntegral m * y) .*^ im)

tnm :: RealFloat a => Int -> Int -> a -> a -> Complex a
tnm n m x y = 0.5 * (enm n m x y + enm (-n) (-m) x y)

wnm :: RealFloat a => Int -> Int -> a -> a -> Complex a
wnm n m x y = (1/3) * (enm n m x y + enm m (-n - m) x y + enm (-n - m) n x y)

--------------------------------------------------------------------------------
generalLattice :: RealFloat a => a -> a -> Int -> Int -> Recipe a
generalLattice xi eta n m (x :+ y) = enm n m x' y'
  where
    x' = x - xi * y / eta
    y' = y / eta

p1 :: RealFloat a => a -> a -> [Coef a] -> Recipe a
p1 xi eta =  mkRecipe (generalLattice xi eta)

p2 :: RealFloat a => a -> a -> [Coef a] -> Recipe a
p2 xi eta cs = mkRecipe (generalLattice xi eta) (nub $cs ++ (negateCoefs <$> cs))

--------------------------------------------------------------------------------
rhombicLattice :: RealFloat a => a -> Int -> Int -> Recipe a
rhombicLattice b n m (x :+ y) = enm n m x' y'
  where
    x' = x + y / (2*b)
    y' = x - y / (2*b)

cm :: RealFloat a => a -> [Coef a] -> Recipe a
cm b cs = mkRecipe (rhombicLattice b) (nub $ cs ++ (reverseCoefs <$> cs))

cmm :: RealFloat a => a -> [Coef a] -> Recipe a
cmm b cs = mkRecipe (rhombicLattice b) (nub $ cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs <$> cs
    cs2 = reverseCoefs <$> cs
    cs3 = reverseCoefs . negateCoefs <$> cs

--------------------------------------------------------------------------------
rectangularLattice :: RealFloat a => a -> Int -> Int -> Recipe a
rectangularLattice l n m (x :+ y) = enm n m x (y / l)

rectangularLattice' :: RealFloat a => a -> Int -> Int -> Recipe a
rectangularLattice' l n m (x :+ y) = tnm n m x (y / l)

pm :: RealFloat a => a -> [Coef a] -> Recipe a
pm l cs = mkRecipe (rectangularLattice l) (nub $ cs ++ (negateSnd <$> cs))

pg :: RealFloat a => a -> [Coef a] -> Recipe a
pg l cs = mkRecipe (rectangularLattice l) (nub $ cs ++ cs')
  where
    cs' = negateSnd . alternateCoefs (\n _ -> (-1) ^ n) <$> cs

pmm :: RealFloat a => a -> [Coef a] -> Recipe a
pmm l cs = mkRecipe (rectangularLattice l) (nub $ cs ++ (negateSnd <$> cs))

pmg :: RealFloat a => a -> [Coef a] -> Recipe a
pmg l cs = mkRecipe (rectangularLattice' l) (nub $ cs ++ (negateSnd <$> cs))

pgg :: RealFloat a => a -> [Coef a] -> Recipe a
pgg l cs = mkRecipe (rectangularLattice' l) (nub $ cs ++ cs')
  where
    cs' = negateSnd . alternateCoefs (\n m -> (-1) ^ (n+m)) <$> cs

--------------------------------------------------------------------------------
squareLattice :: RealFloat a => Int -> Int -> Recipe a
squareLattice n m (x :+ y) = 0.5 * (tnm n m x y + tnm (-n) m x y)

p4 :: RealFloat a => [Coef a] -> Recipe a
p4 = mkRecipe squareLattice

p4m :: RealFloat a => [Coef a] -> Recipe a
p4m cs = mkRecipe squareLattice (nub $ cs ++ (reverseCoefs <$> cs))

p4g :: RealFloat a => [Coef a] -> Recipe a
p4g cs = mkRecipe squareLattice (nub $ cs ++ cs')
  where
    cs' = reverseCoefs . alternateCoefs (\n m -> (-1) ^ (n+m)) <$> cs

--------------------------------------------------------------------------------
hexagonalLattice :: RealFloat a => Int -> Int -> Recipe a
hexagonalLattice n m (x :+ y) = (1/3) * (enm n m x' y' + enm m (-n - m) x' y' + enm (-n - m) n x' y')
  where
    x' = x + y / sqrt3
    y' = 2 * y / sqrt3
    sqrt3 = sqrt 3

p3 :: RealFloat a => [Coef a] -> Recipe a
p3 = mkRecipe hexagonalLattice

p31m :: RealFloat a => [Coef a] -> Recipe a
p31m cs = mkRecipe hexagonalLattice (nub $ cs ++ (reverseCoefs <$> cs))

p3m1 :: RealFloat a => [Coef a] -> Recipe a
p3m1 cs = mkRecipe hexagonalLattice (nub $ cs ++ (negateCoefs . reverseCoefs <$> cs))

p6 :: RealFloat a => [Coef a] -> Recipe a
p6 cs = mkRecipe hexagonalLattice (nub $ cs ++ (negateCoefs <$> cs))

p6m :: RealFloat a => [Coef a] -> Recipe a
p6m cs = mkRecipe hexagonalLattice (nub $ cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCoefs <$> cs
    cs2 = reverseCoefs <$> cs
    cs3 = negateCoefs <$> cs2
