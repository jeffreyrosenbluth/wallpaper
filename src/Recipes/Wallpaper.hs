module Recipes.Wallpaper where

import           Types
import           Complextra

import           Data.Complex

exp2pii :: Double -> Complex Double
exp2pii x = exp (2 * pi * x .*^ im)

enm :: Int -> Int -> Double -> Double -> Complex Double
enm n m x y = exp (2 * pi * (fromIntegral n * x + fromIntegral m * y) .*^ im)

tnm :: Int -> Int -> Double -> Double -> Complex Double
tnm n m x y = 0.5 * (enm n m x y + enm (-n) (-m) x y)

snm :: Int -> Int -> Double -> Double -> Complex Double
snm n m x y = 0.5 * (tnm n m x y + tnm (-n) m x y)

neg :: Num a => (a, a) -> (a, a)
neg (a,b) = (negate a, negate b)

neg2nd :: Num b => (a, b) -> (a, b)
neg2nd (a, b) = (a, negate b)

neg1st :: Num a => (a, b) -> (a, b)
neg1st (a, b) = (negate a, b)

rev :: (a, a) -> (a, a)
rev (a, b) = (b, a)

--------------------------------------------------------------------------------
generalLattice :: Double -> Double -> Int -> Int -> Recipe
generalLattice xi eta n m (x :+ y) = enm n m x' y'
  where
    x' = x - xi * y / eta
    y' = y / eta

p1 :: Double -> Double -> [Coefficent] -> Recipe
p1 xi eta cs z = sum $ zipWith (*) as es
  where
    as = snd <$> cs
    es = ($ z) . uncurry (generalLattice xi eta) <$> (fst <$> cs)

p2 :: Double -> Double -> [Coefficent] -> Recipe
p2 xi eta cs z = sum $ zipWith (*) (as ++ as) (es ++ es')
  where
    as  = snd <$> cs
    es  = ($ z) . uncurry (generalLattice xi eta) <$> (fst <$> cs)
    es' = ($ z) . uncurry (generalLattice xi eta) <$> (neg . fst <$> cs)

--------------------------------------------------------------------------------
rhombicLattice :: Double -> Int -> Int -> Recipe
rhombicLattice b n m (x :+ y) = enm n m x' y'
  where
    x' = x + y / (2*b)
    y' = x - y / (2*b)

cm :: Double -> [Coefficent] -> Recipe
cm b cs z = sum $ zipWith (*) (as ++ as) (es ++ es')
  where
    as  = snd <$> cs
    es  = ($ z) . uncurry (rhombicLattice b) <$> (fst <$> cs)
    es' = ($ z) . uncurry (rhombicLattice b) <$> (rev . fst <$> cs)

cmm :: Double -> [Coefficent] -> Recipe
cmm b cs z = sum $ zipWith (*) (cycle as) (es1 ++ es2 ++ es3 ++ es4)
  where
    as = snd <$> cs
    es1 = ($ z) . uncurry (rhombicLattice b) <$> (fst <$> cs)
    es2 = ($ z) . uncurry (rhombicLattice b) <$> (neg . fst <$> cs)
    es3 = ($ z) . uncurry (rhombicLattice b) <$> (rev . fst <$> cs)
    es4 = ($ z) . uncurry (rhombicLattice b) <$> (rev . neg . fst <$> cs)

--------------------------------------------------------------------------------
rectangularLattice :: Double -> Int -> Int -> Recipe
rectangularLattice l n m (x :+ y) = enm n m x (y / l)

rectangularLattice' :: Double -> Int -> Int -> Recipe
rectangularLattice' l n m (x :+ y) = tnm n m x (y / l)

pm :: Double -> [Coefficent] -> Recipe
pm l cs z = sum $ zipWith (*) (as ++ as) (es ++ es')
  where
    as  = snd <$> cs
    es  = ($ z) . uncurry (rectangularLattice l) <$> (fst <$> cs)
    es' = ($ z) . uncurry (rectangularLattice l) <$> (neg2nd . fst <$> cs)

pg :: Double -> [Coefficent] -> Recipe
pg l cs z = sum $ zipWith (*) (as ++ as') (es ++ es')
  where
    sgns = (\n -> (-1) ^ n) . fst . fst <$> cs
    as   = snd <$> cs
    as'  = zipWith (*) sgns as
    es   = ($ z) . uncurry (rectangularLattice l) <$> (fst <$> cs)
    es'  = ($ z) . uncurry (rectangularLattice l) <$> (neg2nd . fst <$> cs)

pmm :: Double -> [Coefficent] -> Recipe
pmm l cs z = sum $ zipWith (*) (as ++ as) (es ++ es')
  where
    as  = snd <$> cs
    es  = ($ z) . uncurry (rectangularLattice' l) <$> (fst <$> cs)
    es' = ($ z) . uncurry (rectangularLattice' l) <$> (neg2nd . fst <$> cs)

pmg :: Double -> [Coefficent] -> Recipe
pmg l cs z = sum $ zipWith (*) (as ++ as') (es ++ es')
  where
    sgns = (\n -> (-1) ^ n) . fst . fst <$> cs
    as   = snd <$> cs
    as'  = zipWith (*) sgns as
    es   = ($ z) . uncurry (rectangularLattice' l) <$> (fst <$> cs)
    es'  = ($ z) . uncurry (rectangularLattice' l) <$> (neg2nd . fst <$> cs)

pgg :: Double -> [Coefficent] -> Recipe
pgg l cs z = sum $ zipWith (*) (as ++ as') (es ++ es')
  where
    sgns = (\n -> (-1) ^ n) . (\(a,b) -> a + b) . fst <$> cs
    as   = snd <$> cs
    as'  = zipWith (*) sgns as
    es   = ($ z) . uncurry (rectangularLattice' l) <$> (fst <$> cs)
    es'  = ($ z) . uncurry (rectangularLattice' l) <$> (neg2nd . fst <$> cs)
--------------------------------------------------------------------------------
squareLattice :: Int -> Int -> Recipe
squareLattice m n (x :+ y) = snm n m x y

p4 :: [Coefficent] -> Recipe
p4 cs z = sum $ zipWith (*) as es
  where
    as = snd <$> cs
    es = ($ z) . uncurry squareLattice <$> (fst <$> cs)

p4m :: [Coefficent] -> Recipe
p4m cs z = sum $ zipWith (*) (as ++ as) (es ++ es')
  where
    as  = snd <$> cs
    es  = ($ z) . uncurry squareLattice <$> (fst <$> cs)
    es' = ($ z) . uncurry squareLattice <$> (rev . fst <$> cs)
