module Main where

import           Core
import           Types
import           Juicy
import           Recipe
import           Recipes.Wallpaper
import           Recipes.Frieze

import           Data.Complex
import           Codec.Picture
import           System.Environment

main :: IO ()
main = do
  [inFile, outFile] <- getArgs
  wallpaper (Wallpaper defaultOpts P4G coefs) inFile outFile

coefs :: [Coef Double]
coefs = [ Coef 1 0 (0.75:+0.25)
        , Coef (-2) 2 (0.2:+(-0.2))
        , Coef 1 (-1) (0.6:+0.1)
        ]

wallpaper :: RealFloat a => Wallpaper a -> FilePath -> FilePath -> IO ()
wallpaper wp inFile outFile = do
  dImg              <- readImage inFile
  let img  = case dImg of
         Left e -> error e
         Right i -> symmetry opts rcp (toImageRGBA8 i)
  writeImage outFile img
  where
    opts = wpOptions wp
    rcp  = recipe (wpGroup wp) (wpCoefs wp)

