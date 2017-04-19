module Main where

import           Core
import           Types
import           Juicy
import           Recipes.Wallpaper

import           Data.Complex
import           Codec.Picture
import           System.Environment
import           System.FilePath    (takeExtension)

main :: IO ()
main = do
  [inFile, outFile] <- getArgs
  dImg <- readImage inFile
  let img  = case dImg of
         Left e -> error e
         Right i -> below' $ toImageRGBA8 i
  case takeExtension outFile of
     ".png" -> writePng outFile img
     ".tif" -> writeTiff outFile img
     ".bmp" -> writeBitmap outFile img
     ".jpg" -> writeJpeg 80 outFile img
     _      -> writePng outFile img
  where
    below' a = below a a

tr :: (Pixel p, BlackWhite p) => Image p -> Image p
tr = transform opts (p3m1 coefs)
  where
    opts :: Options Double
    opts = defaultOpts {width=750, height=750, repLength=200, scale=0.5}

coefs :: [Coef Double]
coefs = [ Coef 1 0 (0.75:+0.25)
        , Coef (-2) 2 (0.2:+(-0.2))
        , Coef 1 (-1) (0.6:+0.1)
        ]

