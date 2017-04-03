module Main where

import           Lib
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
  let tr = morph (0.5 :: Double) 0.75 0.2 1000 500
             (p6 $ mkCoef <$> [(1,0,0.75:+0.25), (3,1, 0.6:+(-0.2)), (1,(-1),0.2:+0.1)])
  writePng "output.png" $ case dImg of
    Right img -> tr (toImageRGBA8 img)
    Left e -> error e

