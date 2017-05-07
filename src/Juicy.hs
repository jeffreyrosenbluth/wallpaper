{-# LANGUAGE ScopedTypeVariables #-}

---------------------------------------------------------------------------------
-- |
-- Module      :  Juicy
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Tools for creating symmtery images specific to JuicyPixels.
--
-- <<examples/squares.png>>
---------------------------------------------------------------------------------

module Juicy
  (
   -- * Wallpaper Generation
    symmetryPattern
  , rosettePattern
  , portrait
  , symmetryPortrait

  -- * Color Wheels
  , colorWheel
  , colorWheelP

  -- * Image Processing
  , invertImage
  , flipVertical
  , flipHorizontal
  , flipBoth
  , beside
  , below
  , antiSymmHorizontal
  , antiSymmVertical

  -- * JuicyPixels Utilities
  , toImageRGBA8
  , writeJpeg
  , writeImage
  ) where

import           Core
import           Recipes.Common
import           Recipes.Rosette
import           Types

import           Codec.Picture
import           Codec.Picture.Types
import qualified Data.ByteString.Lazy as L (writeFile)
import           Data.Complex
import           Data.Word            (Word8)
import           System.FilePath      (takeExtension)

-- Wallpaper Generation --------------------------------------------------------

portrait :: RealFloat a
         => Options a
         -> Recipe a
         -> FilePath
         -> IO ()
portrait opts f outFile = writeImage outFile $ symmFromFn opts f colorWheelP


symmetryPortrait :: RealFloat a
                 => Options a
                 -> ([Coef a] -> Recipe a)
                 -> [Coef a]
                 -> FilePath
                 -> IO ()
symmetryPortrait opts rf cs outFile =
  writeImage outFile $ symmFromFn opts (rf cs) colorWheel

-- | Crate a wallpaper and write it to an output file.
symmetryPattern :: RealFloat a
                => Options a
                -> ([Coef a] -> Recipe a)
                -> [Coef a]
                -> WPtype a
                -> PreProcess
                -> FilePath
                -> FilePath
                -> IO ()
symmetryPattern opts rf cs typ pp inFile outFile = do
  dImg <- readImage inFile
  let img  = case dImg of
         Left e  -> error e
         Right i ->  let img' = preProcess pp . toImageRGBA8 $ i
                     in case typ of
           Plain   -> symmetry opts (rf cs) img'
           Morph c -> morph opts (rf cs) c img'
           Blend g -> blend opts (rf cs) (recipe g cs) img'
  writeImage outFile img

rosettePattern :: RealFloat a
                => Options a
                -> [Coef a]
                -> Int
                -> Bool
                -> PreProcess
                -> FilePath
                -> FilePath
                -> IO ()
rosettePattern opts cs pfold mirror pp inFile outFile = do
  dImg <- readImage inFile
  let img  = case dImg of
         Left e  -> error e
         Right i -> let img' = preProcess pp . toImageRGBA8 $ i
                    in  if mirror
                          then symmetry opts (rosettePM pfold cs) img'
                          else symmetry opts (rosetteP pfold cs) img'
  writeImage outFile img

-- | Convert a hue in radians (-pi, pi] to RGB
hue :: forall a. RealFloat a => a -> PixelRGBA8
hue radians = case hi of
  0 -> PixelRGBA8 255 t 0 255
  1 -> PixelRGBA8 q 255 0 255
  2 -> PixelRGBA8 0 255 t 255
  3 -> PixelRGBA8 0 q 255 255
  4 -> PixelRGBA8 t 0 255 255
  5 -> PixelRGBA8 255 0 q 255
  _ -> error "The sky is falling, mod 6 can only be [0,5]"
  where
    rad = if radians <= 0 then radians + 2 * pi else radians
    degrees = 360 * rad / (2 * pi)
    hi :: Int
    hi = floor (degrees/60) `mod` 6
    t =  round $ 255 * mod1 (degrees/60)
    q = 255 - t
    mod1 x
      | f < 0 = f + 1
      | otherwise = f
      where
        (_, f) :: (Int, a) = properFraction x

-- | A Color wheel on the entire complex plane.
--   The color is solely based on the phase of the complex number.
colorWheel :: RealFloat a => Complex a -> PixelRGBA8
colorWheel z = hue (phase z)

colorWheelP :: RealFloat a => Complex a -> PixelRGBA8
colorWheelP z = if (==) (fromIntegral . round $ degrees) degrees then black else hue rad
  where
    radians = phase z
    rad = if radians <= 0 then radians + 2 * pi else radians
    degrees =  360 * rad / (2 * pi)

preProcess :: (Pixel p, Invertible p) => PreProcess -> Image p -> Image p
preProcess process = case process of
  FlipHorizontal     -> flipHorizontal
  FlipVertical       -> flipVertical
  FlipBoth           -> flipBoth
  Invert             -> invertImage
  AntiSymmHorizontal -> antiSymmHorizontal
  AntiSymmVertical   -> antiSymmVertical
  None               -> id

-- Image Processing -------------------------------------------------------------

invertImage :: (Pixel p, Invertible p) => Image p -> Image p
invertImage = pixelMap invert

flipHorizontal :: Pixel a => Image a -> Image a
flipHorizontal img@(Image w h _) = generateImage g  w h
  where
    g x = pixelAt img (w - 1 - x)
{-# INLINEABLE flipHorizontal #-}

flipVertical :: Pixel a => Image a -> Image a
flipVertical img@(Image w h _) = generateImage g w h
  where
    g x y = pixelAt img x (h - 1 - y)
{-# INLINEABLE flipVertical #-}

flipBoth :: Pixel a => Image a -> Image a
flipBoth img@(Image w h _) = generateImage g w h
  where
    g x y = pixelAt img (w - 1 - x) (h - 1 - y)
{-# INLINEABLE flipBoth #-}

beside :: Pixel a => Image a -> Image a -> Image a
beside img1@(Image w1 h _) img2@(Image w2 _ _) =
  generateImage g (w1 + w2) h
  where
    g x
      | x < w1 = pixelAt img1 x
      | otherwise = pixelAt img2 (x - w1)
{-# INLINEABLE beside #-}

below :: Pixel a => Image a -> Image a -> Image a
below img1@(Image w h1 _) img2@(Image _ h2 _) =
  generateImage g w (h1 + h2)
  where
    g x y
      | y < h1 = pixelAt img1 x y
      | otherwise = pixelAt img2 x (y - h1)
{-# INLINEABLE below #-}

antiSymmHorizontal :: (Pixel a, Invertible a) => Image a -> Image a
antiSymmHorizontal img = below img (flipHorizontal . invertImage $ img)
{-# INLINEABLE antiSymmHorizontal #-}

antiSymmVertical :: (Pixel a, Invertible a) => Image a -> Image a
antiSymmVertical img = beside img (flipVertical . invertImage $ img)
{-# INLINEABLE antiSymmVertical #-}

-- Utilities --------------------------------------------------------------------

toImageRGBA8 :: DynamicImage -> Image PixelRGBA8
toImageRGBA8 (ImageRGBA8 i)  = i
toImageRGBA8 (ImageRGB8 i)   = promoteImage i
toImageRGBA8 (ImageYCbCr8 i) = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 (ImageY8 i)     = promoteImage i
toImageRGBA8 (ImageYA8 i)    = promoteImage i
toImageRGBA8 (ImageCMYK8 i)  = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 _               = error "Unsupported Pixel type"

writeJpeg :: Word8 -> FilePath -> Image PixelRGBA8 -> IO ()
writeJpeg quality outFile img = L.writeFile outFile bs
  where
    bs = encodeJpegAtQuality quality (pixelMap (convertPixel . dropTransparency) img)

-- | Write an image file to disk, the image type depends on the file extension
--   of the output file name.
writeImage :: FilePath -> Image PixelRGBA8 -> IO ()
writeImage outFile img =
  case takeExtension outFile of
     ".png" -> writePng outFile img
     ".tif" -> writeTiff outFile img
     ".bmp" -> writeBitmap outFile img
     ".jpg" -> writeJpeg 80 outFile img
     _      -> writePng outFile img
