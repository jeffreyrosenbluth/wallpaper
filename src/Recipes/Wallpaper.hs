module Recipes.Wallpaper where

import           Types
import           Complextra

import           Data.Complex

mkCoefficient :: (Int, Int, Complex a) -> Coefficient a
mkCoefficient (n, m, a) = Coefficient n m a

lattice :: RealFloat a => (Int -> Int -> Recipe a) -> [Coefficient a] -> Recipe a
lattice mkRecipe cs z = sum $ zipWith (*) as rs
  where
    as = anm <$> cs
    rs = ($ z) . uncurry mkRecipe <$> [(nCoord c, mCoord c) | c <- cs]

exp2pii :: RealFloat a => a -> Complex a
exp2pii x = exp (2 * pi * x .*^ im)

enm :: RealFloat a => Int -> Int -> a -> a -> Complex a
enm n m x y = exp (2 * pi * (fromIntegral n * x + fromIntegral m * y) .*^ im)

tnm :: RealFloat a => Int -> Int -> a -> a -> Complex a
tnm n m x y = 0.5 * (enm n m x y + enm (-n) (-m) x y)

snm :: RealFloat a => Int -> Int -> a -> a -> Complex a
snm n m x y = 0.5 * (tnm n m x y + tnm (-n) m x y)

negateCnm :: Coefficient a -> Coefficient a
negateCnm (Coefficient n m a) = Coefficient (-n) (-m) a

negateCm :: Coefficient a -> Coefficient a
negateCm (Coefficient n m a) = Coefficient n (-m) a

reverseCnm :: Coefficient a -> Coefficient a
reverseCnm (Coefficient n m a) = Coefficient m n a

alternateCanm :: RealFloat a => (Int -> Int -> Int) -> Coefficient a -> Coefficient a
alternateCanm alt (Coefficient n m a) = Coefficient n m (fromIntegral (alt n m) .*^ a)

--------------------------------------------------------------------------------
generalLattice :: RealFloat a => a -> a -> Int -> Int -> Recipe a
generalLattice xi eta n m (x :+ y) = enm n m x' y'
  where
    x' = x - xi * y / eta
    y' = y / eta

p1 :: RealFloat a => a -> a -> [Coefficient a] -> Recipe a
p1 xi eta =  lattice (generalLattice xi eta)

p2 :: RealFloat a => a -> a -> [Coefficient a] -> Recipe a
p2 xi eta cs = lattice (generalLattice xi eta) (cs ++ (negateCnm <$> cs))

--------------------------------------------------------------------------------
rhombicLattice :: RealFloat a => a -> Int -> Int -> Recipe a
rhombicLattice b n m (x :+ y) = enm n m x' y'
  where
    x' = x + y / (2*b)
    y' = x - y / (2*b)

cm :: RealFloat a => a -> [Coefficient a] -> Recipe a
cm b cs = lattice (rhombicLattice b) (cs ++ (reverseCnm <$>) cs)

cmm :: RealFloat a => a -> [Coefficient a] -> Recipe a
cmm b cs = lattice (rhombicLattice b) (cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCnm <$> cs
    cs2 = reverseCnm <$> cs
    cs3 = (reverseCnm . negateCnm) <$> cs

--------------------------------------------------------------------------------
rectangularLattice :: RealFloat a => a -> Int -> Int -> Recipe a
rectangularLattice l n m (x :+ y) = enm n m x (y / l)

rectangularLattice' :: RealFloat a => a -> Int -> Int -> Recipe a
rectangularLattice' l n m (x :+ y) = tnm n m x (y / l)

pm :: RealFloat a => a -> [Coefficient a] -> Recipe a
pm l cs = lattice (rectangularLattice l) (cs ++ (negateCm <$> cs))

pg :: RealFloat a => a -> [Coefficient a] -> Recipe a
pg l cs = lattice (rectangularLattice l) (cs ++ cs')
  where
    cs' = (alternateCanm (\n _ -> (-1) ^ n) . negateCm) <$> cs

pmm :: RealFloat a => a -> [Coefficient a] -> Recipe a
pmm l cs = lattice (rectangularLattice l) (cs ++ (negateCm <$> cs))

pmg :: RealFloat a => a -> [Coefficient a] -> Recipe a
pmg l cs = lattice (rectangularLattice' l) (cs ++ (negateCm <$> cs))

pgg :: RealFloat a => a -> [Coefficient a] -> Recipe a
pgg l cs = lattice (rectangularLattice' l) (cs ++ cs')
  where
    cs' = (alternateCanm (\n m -> (-1) ^ (n+m)) . negateCm) <$> cs

--------------------------------------------------------------------------------
squareLattice :: RealFloat a => Int -> Int -> Recipe a
squareLattice m n (x :+ y) = snm n m x y

p4 :: RealFloat a => [Coefficient a] -> Recipe a
p4 = lattice squareLattice

p4m :: RealFloat a => [Coefficient a] -> Recipe a
p4m cs = lattice squareLattice (cs ++ (reverseCnm <$> cs))
