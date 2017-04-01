module Recipes.Wallpaper where

import           Types
import           Complextra

import           Data.Complex

mkCoeff :: Coefficent -> Coeff
mkCoeff ((n, m), a) = Coeff n m a

lattice :: (Int -> Int -> Recipe) -> [Coeff] -> Recipe
lattice mkRecipe cs z = sum $ zipWith (*) as rs
  where
    as = anm <$> cs
    rs = ($ z) . uncurry mkRecipe <$> [(nCoord c, mCoord c) | c <- cs]

exp2pii :: Double -> Complex Double
exp2pii x = exp (2 * pi * x .*^ im)

enm :: Int -> Int -> Double -> Double -> Complex Double
enm n m x y = exp (2 * pi * (fromIntegral n * x + fromIntegral m * y) .*^ im)

tnm :: Int -> Int -> Double -> Double -> Complex Double
tnm n m x y = 0.5 * (enm n m x y + enm (-n) (-m) x y)

snm :: Int -> Int -> Double -> Double -> Complex Double
snm n m x y = 0.5 * (tnm n m x y + tnm (-n) m x y)

negateCnm :: Coeff -> Coeff
negateCnm (Coeff n m a) = Coeff (-n) (-m) a

negateCm :: Coeff -> Coeff
negateCm (Coeff n m a) = Coeff n (-m) a

reverseCnm :: Coeff -> Coeff
reverseCnm (Coeff n m a) = Coeff m n a

alternateCanm :: (Int -> Int -> Int) -> Coeff -> Coeff
alternateCanm alt (Coeff n m a) = Coeff n m (fromIntegral (alt n m) .*^ a)

--------------------------------------------------------------------------------
generalLattice :: Double -> Double -> Int -> Int -> Recipe
generalLattice xi eta n m (x :+ y) = enm n m x' y'
  where
    x' = x - xi * y / eta
    y' = y / eta

p1 :: Double -> Double -> [Coeff] -> Recipe
p1 xi eta =  lattice (generalLattice xi eta)

p2 :: Double -> Double -> [Coeff] -> Recipe
p2 xi eta cs = lattice (generalLattice xi eta) (cs ++ (negateCnm <$> cs))

--------------------------------------------------------------------------------
rhombicLattice :: Double -> Int -> Int -> Recipe
rhombicLattice b n m (x :+ y) = enm n m x' y'
  where
    x' = x + y / (2*b)
    y' = x - y / (2*b)

cm :: Double -> [Coeff] -> Recipe
cm b cs = lattice (rhombicLattice b) (cs ++ (reverseCnm <$>) cs)

cmm :: Double -> [Coeff] -> Recipe
cmm b cs = lattice (rhombicLattice b) (cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCnm <$> cs
    cs2 = reverseCnm <$> cs
    cs3 = (reverseCnm . negateCnm) <$> cs

--------------------------------------------------------------------------------
rectangularLattice :: Double -> Int -> Int -> Recipe
rectangularLattice l n m (x :+ y) = enm n m x (y / l)

rectangularLattice' :: Double -> Int -> Int -> Recipe
rectangularLattice' l n m (x :+ y) = tnm n m x (y / l)

pm :: Double -> [Coeff] -> Recipe
pm l cs = lattice (rectangularLattice l) (cs ++ (negateCm <$> cs))

pg :: Double -> [Coeff] -> Recipe
pg l cs = lattice (rectangularLattice l) (cs ++ cs')
  where
    cs' = (alternateCanm (\n _ -> (-1) ^ n) . negateCm) <$> cs

pmm :: Double -> [Coeff] -> Recipe
pmm l cs = lattice (rectangularLattice l) (cs ++ (negateCm <$> cs))

pmg :: Double -> [Coeff] -> Recipe
pmg l cs = lattice (rectangularLattice' l) (cs ++ (negateCm <$> cs))

pgg :: Double -> [Coeff] -> Recipe
pgg l cs = lattice (rectangularLattice' l) (cs ++ cs')
  where
    cs' = (alternateCanm (\n m -> (-1) ^ (n+m)) . negateCm) <$> cs

--------------------------------------------------------------------------------
squareLattice :: Int -> Int -> Recipe
squareLattice m n (x :+ y) = snm n m x y

p4 :: [Coeff] -> Recipe
p4 = lattice squareLattice

p4m :: [Coeff] -> Recipe
p4m cs = lattice squareLattice (cs ++ (reverseCnm <$> cs))
