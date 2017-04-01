module Main where

import           Lib
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
  let tr = transform (0.50 :: Double) 100 700 700 (p31m $ mkCoef <$> [(1, -1, 1 :+ 0)])
  writePng "output.png" $ case dImg of
    Right img -> tr (toImageRGBA8 img)
    Left e -> error "ouch"
  -- writePng "output.png" $ case dImg of
  --   Left msg -> error msg
  --   Right (ImageRGB8  img) -> tr (promoteImage img)
  --   Right (ImageRGBA8 img) -> tr img
  --   Right _                -> error "png images only please."

-- let tr = transform (10**11) (recipe5 (0.003 :+ 0) (0 :+ 0)) --(10**18 :+ 0))
-- let tr = transform  1 p4m
-- let tr = transform 7 (p1 0.5 0.5 [((1,0),1.5:+1), ((0,1), (1):+(-1.5)), ((1,(-1)),2:+2)])
-- let tr = transform 7 (p2 1 1 $ mkCoeff <$> [((1,0),1.5:+1), ((0,1), (1):+(-1.5)), ((1,(-1)),2:+2)])
-- let tr = transform 7 (cm 1 [((1,0),1.5:+1), ((3,1), 2:+(-1.5)), ((1,(-1)),2:+1)])
-- let tr = transform 7 (cmm 1 [((1,0),1.5:+1), ((3,1), 2:+(-1.5)), ((1,(-1)),2:+1)])
-- let tr = transform 3 (pm 2 [((1,0),1.5:+1), ((3,1), 2:+(-1.5)), ((1,(-1)),2:+1)])
-- let tr = transform 3 (pgg 2 [((1,0),1.5:+1), ((3,1), 2:+(-1.5)), ((1,(-1)),2:+1)])

recipe5 :: RealFloat a => Complex a -> Complex a -> Recipe a
recipe5 a b z = z**5 + z'**5
              + a * (z**6 * z' + z * z'**6)
              + b * (z**4 * z'**(-6) + z**(-6) * z'**4)
  where
    z' = conjugate z
