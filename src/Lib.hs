module Lib where

import Codec.Picture
import Codec.Picture.Types
import Data.Complex
import System.Environment

type Recipe = Complex Double -> Complex Double

scaleZ :: RealFloat a => a -> Complex a -> Complex a
scaleZ k z = (k :+ 0) * z

(.*^) :: RealFloat a => a -> Complex a -> Complex a
(.*^) = scaleZ

infixl 7 .*^

im :: Num a => Complex a
im = 0 :+ 1

getPixel :: Image PixelRGBA8 -> Double -> Recipe -> Int -> Int -> PixelRGBA8
getPixel img s rcp i j = clamp i' j'
  where
    (w, h) = (imageWidth img, imageHeight img)
    (w2, h2) = (fromIntegral w / 2, fromIntegral h / 2)
    (x :+ y) = (1/s) .*^ rcp ((fromIntegral i - w2) :+ (fromIntegral j - h2))
    (i', j') = (round x + w `div` 2, round y + h `div` 2)
    clamp m n = if m < 0 || n < 0 || m >= w || n >= h
                  then PixelRGBA8 0 0 0 255 -- opaque black
                  else pixelAt img m n

transform :: Double -> Recipe -> Image PixelRGBA8 -> Image PixelRGBA8
transform s rcp img = generateImage (getPixel img s rcp) w h
  where
    (w, h) = (imageWidth img, imageHeight img)

recipe5 :: Complex Double -> Complex Double -> Recipe
recipe5 a b z = z**5 + z'**5
              + a * (z**6 * z' + z * z'**6)
              + b * (z**4 * z'**(-6) + z**(-6) * z'**4)
  where
    z' = conjugate z

p4m :: Recipe
p4m = (+) <$> squareLattice 1 (-1) <*> squareLattice (-1) 1
  
generalLattice :: Double -> Double -> Int -> Int -> Complex Double -> Complex Double
generalLattice xi eta n m (x :+ y) = exp (2 * pi .*^ im * zeta)
  where
    zeta = fromIntegral n * (x - xi * y / eta) :+ fromIntegral m * y / eta

rhombicLattice :: Double -> Int -> Int -> Complex Double -> Complex Double
rhombicLattice b n m (x :+ y) = exp (2 * pi .*^ im * zeta)
  where
    zeta = fromIntegral n * (x + y / (2*b)) :+ fromIntegral m * (x - y / (2*b))

squareLattice :: Int -> Int -> Complex Double -> Complex Double
squareLattice m n (x :+ y) = (1/4) .*^ (e n m + e m (-n) + e (-n) (-m) + e (-m) n)
  where
    e i j = exp (2 * pi * (fromIntegral i * x + fromIntegral j * y) .*^ im)

run :: IO ()
run = do
  dImg <- readImage . head =<< getArgs
  let tr = transform (10**11) (recipe5 (0.003 :+ 0) (0 :+ 0)) --(10**18 :+ 0))
  -- let tr = transform  (0.001) p4m
  writePng "output.png" $ case dImg of
    Left msg -> error msg
    Right (ImageRGB8  img) -> tr (promoteImage img)
    Right (ImageRGBA8 img) -> tr img
    Right _                -> error "png images only please."
