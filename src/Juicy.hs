{-# LANGUAGE ViewPatterns #-}

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
  , colorWheel
  , wheelColoring

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
import           Numeric              (readHex, showHex)
import           System.FilePath      (takeExtension)
import           System.Random

-- Wallpaper Generation --------------------------------------------------------

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

int2rgb :: Int -> PixelRGB8
int2rgb n = PixelRGB8 r g b
  where
    hex = showHex n ""
    (hr, xs) = splitAt 2 hex
    (hg, hb) = splitAt 2 xs
    [(r, _)] = readHex hr
    [(g, _)] = readHex hg
    [(b, _)] = readHex hb

-- | A Color wheel on the entire complex plane.
colorWheel :: RealFloat a => Complex a -> PixelRGB8
colorWheel (phase -> theta)
  | theta <= -2/3 * pi = int2rgb $ rs !! 0
  | theta <= -1/3 * pi = int2rgb $ rs !! 1
  | theta <= 0    * pi = int2rgb $ rs !! 2
  | theta <=  1/3 * pi = int2rgb $ rs !! 3
  | theta <=  2/3 * pi = int2rgb $ rs !! 4
  | theta <=        pi = int2rgb $ rs !! 5
  | otherwise          = int2rgb $ rs !! 6
  where
    rs = floor . (* 16777215) <$> randomRs (0.1 :: Double, 0.9) (mkStdGen 0)

preProcess :: (Pixel p, Invertible p) => PreProcess -> Image p -> Image p
preProcess process = case process of
  FlipHorizontal     -> flipHorizontal
  FlipVertical       -> flipVertical
  FlipBoth           -> flipBoth
  Invert             -> invertImage
  AntiSymmHorizontal -> antiSymmHorizontal
  AntiSymmVertical   -> antiSymmVertical
  None               -> id

wheelColoring :: RealFloat a => Options a -> Recipe a -> Image PixelRGB8
wheelColoring opts rcp =
  generateImage (\i j -> colorWheel . get $ (fromIntegral i :+ fromIntegral j))
                (width opts)
                (height opts)
  where
    get = focusIn (width opts) (height opts) (repLength opts) rcp

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
