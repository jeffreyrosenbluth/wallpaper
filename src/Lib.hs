module Lib where

import           Recipes
import           Recipes.Frieze
import           Recipes.Wallpaper
import           Types
import           Complextra

import           Codec.Picture
import           Data.Complex


getPixel' :: (RealFloat a, Pixel p, BlackWhite p) => Image p -> Int -> Int -> a -> a
         -> Recipe a -> Int -> Int -> p
getPixel' img w2 h2 s t rcp i j = clamp (round x + w1 `div` 2) (round y + h1 `div` 2)
  where
    (w1, h1) = (imageWidth img, imageHeight img)
    (x :+ y) = (s * 0.5 * fromIntegral (min w1 h1))
            .*^ focusIn w2 h2 rcp (fromIntegral i / t :+ fromIntegral j / t)
    clamp m n
      | m < 0 || n < 0 || m >= w1 || n >= h1 = black
      | otherwise = pixelAt img m n

mkRecipe :: RealFloat a => Options a -> Recipe a
mkRecipe opts = rcp (coefs opts)
  where
    rcp = case symmetry opts of
      F P111        -> p111
      F P211        -> p211
      F P1M1        -> p1m1
      F P11M        -> p11m
      F P11G        -> p11g
      F P2MM        -> p2mm
      F P2MG        -> p2mg
      W (P1 xi eta) -> p1 xi eta
      W (P2 xi eta) -> p2 xi eta
      W (CM b)      -> cm b
      W (CMM b)     -> cmm b
      W (PM l)      -> pm l
      W (PG l)      -> pg l
      W (PMM l)     -> pmm l
      W (PMG l)     -> pmg l
      W (PGG l)     -> pgg l
      W P4          -> p4
      W P4M         -> p4m
      W P4G         -> p4g
      W P3          -> p3
      W P31M        -> p31m
      W P3M1        -> p3m1
      W P6          -> p6
      W P6M         -> p6m
      R _           -> error "Rosette not yet implemented"

getPixel :: (RealFloat a, Pixel p, BlackWhite p) => Image p -> Options a -> Int -> Int -> p
getPixel img opts i j = clamp (round x + w1 `div` 2) (round y + h1 `div` 2)
  where
    t        = focus opts
    (w1, h1) = (imageWidth img, imageHeight img)
    (x :+ y) = (scale opts * 0.5 * fromIntegral (min w1 h1))
            .*^ focusIn (width opts)
                        (height opts)
                        (mkRecipe opts)
                        (fromIntegral i / t :+ fromIntegral j / t)
    clamp m n
      | m < 0 || n < 0 || m >= w1 || n >= h1 = black
      | otherwise = pixelAt img m n

transform' :: (RealFloat a, Pixel p, BlackWhite p) => a -> a -> Int -> Int -> Recipe a -> Image p
          -> Image p
transform' s t w h rcp img = generateImage (getPixel' img w h s t rcp) w h

transform :: (RealFloat a, Pixel p, BlackWhite p) => Options a -> Image p
          -> Image p
transform opts img = generateImage (getPixel img opts) (width opts) (height opts)

blend :: (RealFloat a, Pixel p, BlackWhite p)
      => a -> a -> Int -> Int -> Recipe a -> Recipe a -> Image p -> Image p
blend s t w h rcp1 rcp2 = transform' s t w h rcp
  where
    rcp z@(x :+ _) = let a = (x + m) / (2 * m)
                     in  a .*^ rcp2 z + (1 - a) .*^ rcp1 z
    m = max 1 (fromIntegral w / fromIntegral h)

morph :: (RealFloat a, Pixel p, BlackWhite p)
      => a -> a -> a -> Int -> Int -> Recipe a -> Image p -> Image p
morph s t c w h rcp = transform' s t w h rcp'
  where
    rcp' z@(x :+ _) = exp (pi * phi c ((x+1)/m) .*^ im) * rcp z 
    m = max 1 (fromIntegral w / fromIntegral h)
    phi cut u
      | u < cut = 1
      | u > 1 - cut = -1
      | otherwise = (-2 / (1 - 2 * cut)) * (u - 0.5)
