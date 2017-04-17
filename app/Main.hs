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
    Right img -> tr (toImageRGBA8 img)
    Left e -> error e

tr :: (Pixel p, BlackWhite p) => Image p -> Image p
tr = morph opts (p4m coefs) 0
  where
    opts = defaultOpts {width = 1500, height=500}

coefs :: [Coef Double]
coefs = [ Coef 1 0 (0.75:+0.25)
        , Coef 3 1 (0.6:+(-0.2))
        , Coef 1 (-1) (0.2:+0.1)
        ]
