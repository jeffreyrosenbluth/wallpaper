module Recipes.Frieze where

import           Types
import           Complextra
import           Recipe

import           Data.Complex

nm :: RealFloat a => Int -> Int -> Recipe a
nm n m z = exp (fromIntegral n .*^ (im * z))
         * exp (fromIntegral m .*^ (im * conjugate z))

p111 :: RealFloat a => [Coef a] -> Recipe a
p111  = wallpaper nm

p211 :: RealFloat a => [Coef a] -> Recipe a
p211 cs = wallpaper nm (cs ++ (negateCnm <$> cs))


p1m1 :: RealFloat a => [Coef a] -> Recipe a
p1m1 cs = wallpaper nm (cs ++ (reverseCnm <$> cs))

p11m :: RealFloat a => [Coef a] -> Recipe a
p11m cs = wallpaper nm (cs ++ (negateCnm . reverseCnm <$> cs))

p11g :: RealFloat a => [Coef a] -> Recipe a
p11g cs = wallpaper nm (cs ++ cs')
  where
    cs' = negateCnm . reverseCnm . alternateCanm (\n m -> (-1) ^ (n+m))<$> cs

p2mm :: RealFloat a => [Coef a] -> Recipe a
p2mm cs = wallpaper nm (cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCnm <$> cs
    cs2 = reverseCnm <$> cs
    cs3 = negateCnm <$> cs2

p2mg :: RealFloat a => [Coef a] -> Recipe a
p2mg cs = wallpaper nm (cs ++ cs1 ++ cs2 ++ cs3)
  where
    cs1 = negateCnm . reverseCnm . alternateCanm (\n m -> (-1) ^ (n+m)) <$> cs
    cs2 = negateCnm <$> cs
    cs3 = reverseCnm . alternateCanm (\n m -> (-1) ^ (n+m)) <$> cs
