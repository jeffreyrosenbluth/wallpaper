module Main where

import           Core
import           Types
import           Recipes.Frieze
import           Recipes.Wallpaper

import           Data.Complex
import           Codec.Picture.Types
import           Codec.Picture
import           System.Environment

toImageRGBA8 :: DynamicImage -> Image PixelRGBA8
toImageRGBA8 (ImageRGBA8 i)  = i
toImageRGBA8 (ImageRGB8 i)   = promoteImage i
toImageRGBA8 (ImageYCbCr8 i) = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 (ImageY8 i)     = promoteImage i
toImageRGBA8 (ImageYA8 i)    = promoteImage i
toImageRGBA8 (ImageCMYK8 i)  = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 _               = error "Unsupported Pixel type"

main :: IO ()
main = do
  dImg <- readImage . head =<< getArgs
  writePng "output.png" $ case dImg of
    -- Right img -> tr (toImageRGBA8 img)
    Right img -> below (toImageRGBA8 img) (negative . flipVH $ toImageRGBA8 img)
    Left e -> error e

tr :: (Pixel p, BlackWhite p) => Image p -> Image p
tr = transform opts (p4g coefs)
  where
    opts = defaultOpts {repLength=200, scale=0.45}

coefs :: [Coef Double]
coefs = [ Coef 1 0 (0.75:+0.25)
        , Coef (-2) 2 (0.2:+(-0.2))
        , Coef 1 (-1) (0.6:+0.1)
        ]

negative :: (Pixel p, Invertible p) => Image p -> Image p
negative = pixelMap invert

flipHorizontally :: Pixel a => Image a -> Image a
flipHorizontally img@(Image w h _) = generateImage gen  w h 
  where
    gen x = pixelAt img (w - 1 - x)

-- | Flip image vertically.

flipVertically :: Pixel a => Image a -> Image a
flipVertically img@(Image w h _) = generateImage gen w h
  where
    gen x y = pixelAt img x (h - 1 - y)

flipVH :: Pixel a => Image a -> Image a
flipVH img@(Image w h _) = generateImage gen w h
  where
    gen x y = pixelAt img (w - 1 - x) (h - 1 - y)

beside :: Pixel a => Image a -> Image a -> Image a
beside img1@(Image w1 h _) img2@(Image w2 _ _) =
  generateImage gen (w1 + w2) h 
  where
    gen x 
      | x < w1 = pixelAt img1 x
      | otherwise = pixelAt img2 (x - w1)

below :: Pixel a => Image a -> Image a -> Image a
below img1@(Image w h1 _) img2@(Image _ h2 _) =
  generateImage gen w (h1 + h2)
  where
    gen x y 
      | y < h1 = pixelAt img1 x y
      | otherwise = pixelAt img2 x (y - h1)
