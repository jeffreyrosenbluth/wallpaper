{-# LANGUAGE ViewPatterns #-}

module Juicy where

import           Core
import           Types
import           Recipe

import           Codec.Picture
import           Codec.Picture.Types
import qualified Data.ByteString.Lazy as L (writeFile)
import           Data.Word            (Word8)
import           Data.Complex
import           System.FilePath      (takeExtension)

symmetryPattern :: RealFloat a
                  => Options a
                  -> ([Coef a] -> Recipe a)
                  -> [Coef a]
                  -> WPtype a
                  -> FilePath
                  -> FilePath
                  -> IO ()
symmetryPattern opts rf cs typ inFile outFile = do
  dImg <- readImage inFile
  let img  = case dImg of
         Left e  -> error e
         Right i -> case typ of
           Plain -> symmetry opts (rf cs) (toImageRGBA8 i)
           Morph c -> morph opts (rf cs) c (toImageRGBA8 i)
           Blend g -> blend opts (rf cs) (recipe g cs) (toImageRGBA8 i)
  writeImage outFile img

colorWheel :: RealFloat a => Complex a -> PixelRGB8
colorWheel (phase -> theta)
  | theta <= -2/5 * pi = PixelRGB8 25 52 65
  | theta <= -1/5 * pi = PixelRGB8 62 96 111
  | theta <=  1/5 * pi = PixelRGB8 145 170 157
  | theta <=  2/5 * pi = PixelRGB8 209 219 189
  | theta <=        pi = PixelRGB8 252 255 245
  | otherwise          = PixelRGB8 255 255 255

wheelColoring :: RealFloat a => Options a -> Recipe a -> Image PixelRGB8
wheelColoring opts rcp =
  generateImage (\i j -> colorWheel . get $ (fromIntegral i :+ fromIntegral j))
                (width opts)
                (height opts)
  where
    get = focusIn (width opts) (height opts) (repLength opts) rcp

negative :: (Pixel p, Invertible p) => Image p -> Image p
negative = pixelMap invert
 
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
antiSymmHorizontal img = below img (flipHorizontal . negative $ img)
{-# INLINEABLE antiSymmHorizontal #-}

antiSymmVertical :: (Pixel a, Invertible a) => Image a -> Image a
antiSymmVertical img = beside img (flipVertical . negative $ img)
{-# INLINEABLE antiSymmVertical #-}


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

writeImage :: FilePath -> (Image PixelRGBA8) -> IO ()
writeImage outFile img = 
  case takeExtension outFile of
     ".png" -> writePng outFile img
     ".tif" -> writeTiff outFile img
     ".bmp" -> writeBitmap outFile img
     ".jpg" -> writeJpeg 80 outFile img
     _      -> writePng outFile img
