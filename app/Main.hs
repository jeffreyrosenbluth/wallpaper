module Main where

import           Lib
import           Recipes.Frieze
import           Recipes.Wallpaper
import           Types

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
  let tr = transform (10 :: Double) 50 1000 250 (p2mg $ mkCoef <$> [(1,0,1.5:+1), (3,1, 2:+(-1.5)), (1,(-1),2:+1)])
  -- let tr = transform (1 :: Double) 100 1000 250 (p211 $ mkCoef
  --      <$> [ (1, -1, 1 :+ 0)
  --          , (0, 2, 0 :+ 0.5)
           -- ])
  writePng "output.png" $ case dImg of
    Right img -> tr (toImageRGBA8 img)
    Left e -> error "ouch"

recipe5 :: RealFloat a => Complex a -> Complex a -> Recipe a
recipe5 a b z = z**5 + z'**5
              + a * (z**6 * z' + z * z'**6)
              + b * (z**4 * z'**(-6) + z**(-6) * z'**4)
  where
    z' = conjugate z
