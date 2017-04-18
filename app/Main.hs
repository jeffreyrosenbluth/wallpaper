module Main where

import           Core
import           Types
import           Juicy
import           Recipes.Frieze
import           Recipes.Wallpaper

import           Data.Complex
import           Codec.Picture
import           System.Environment

main :: IO ()
main = do
  dImg <- readImage . head =<< getArgs
  writePng "output.png" $ case dImg of
    Right img -> tr . antiSymmVertical $ toImageRGBA8 img
    -- Right img -> antiSymmHorizontal (toImageRGBA8 img)
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

