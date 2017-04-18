module Juicy where

import Types

import Codec.Picture
import Codec.Picture.Types

negative :: (Pixel p, Invertible p) => Image p -> Image p
negative = pixelMap invert

flipHorizontally :: Pixel a => Image a -> Image a
flipHorizontally img@(Image w h _) = generateImage g  w h
  where
    g x = pixelAt img (w - 1 - x)

flipVertically :: Pixel a => Image a -> Image a
flipVertically img@(Image w h _) = generateImage g w h
  where
    g x y = pixelAt img x (h - 1 - y)

flipVH :: Pixel a => Image a -> Image a
flipVH img@(Image w h _) = generateImage g w h
  where
    g x y = pixelAt img (w - 1 - x) (h - 1 - y)

beside :: Pixel a => Image a -> Image a -> Image a
beside img1@(Image w1 h _) img2@(Image w2 _ _) =
  generateImage g (w1 + w2) h
  where
    g x
      | x < w1 = pixelAt img1 x
      | otherwise = pixelAt img2 (x - w1)

below :: Pixel a => Image a -> Image a -> Image a
below img1@(Image w h1 _) img2@(Image _ h2 _) =
  generateImage g w (h1 + h2)
  where
    g x y
      | y < h1 = pixelAt img1 x y
      | otherwise = pixelAt img2 x (y - h1)

antiSymmHorizontal :: (Pixel a, Invertible a) => Image a -> Image a
antiSymmHorizontal img = below img (flipHorizontally . negative $ img)

antiSymmVertical :: (Pixel a, Invertible a) => Image a -> Image a
antiSymmVertical img = beside img (flipHorizontally . negative $ img)

toImageRGBA8 :: DynamicImage -> Image PixelRGBA8
toImageRGBA8 (ImageRGBA8 i)  = i
toImageRGBA8 (ImageRGB8 i)   = promoteImage i
toImageRGBA8 (ImageYCbCr8 i) = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 (ImageY8 i)     = promoteImage i
toImageRGBA8 (ImageYA8 i)    = promoteImage i
toImageRGBA8 (ImageCMYK8 i)  = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 _               = error "Unsupported Pixel type"
