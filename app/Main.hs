module Main where

import           Lib
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
  -- let tr = transform (0.5 :: Double) 0.5 750 750
  --            (p6m $ mkCoef <$> [(1,0,0.75:+0.25), (3,1, 0.6:+(-0.2)), (1,(-1),0.2:+0.1)])
  writePng "output.png" $ case dImg of
    Right img -> tr (toImageRGBA8 img)
    Left e -> error e

tr :: (Pixel p, BlackWhite p) => Image p -> Image p
tr = transform opts
  where
    opts = Options 750 750 0.5 0.5 cs (W P4M)
    cs   = [ Coef 1 0 (0.75:+0.25)
           , Coef 3 1 (0.6:+(-0.2))
           , Coef 1 (-1) (0.2:+0.1)
           ]
