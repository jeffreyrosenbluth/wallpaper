{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Juicy
import           Recipe
import           Recipes.Wallpaper
import           Recipes.Frieze
import           Types

import           Codec.Picture.Types
import           Data.Complex
import           Data.Yaml          (ParseException, decodeFileEither)
import           System.Environment

main :: IO ()
main = do
  -- let wc = wheelColoring (Options 750 750 150 0.5) (p2mg coefs)
  -- writeImage "wheel_out.png" (promoteImage wc)
  [yamlFile] <- getArgs
  (wp :: Either ParseException (Wallpaper Double)) <- decodeFileEither yamlFile
  case wp of
    Left e   -> error (show e)
    Right w  -> wallpaper w

coefs :: [Coef Double]
coefs = [ Coef 1 0 (0.75:+0.25)
        , Coef (-2) 2 (0.2:+(-0.2))
        , Coef 1 (-1) (0.6:+0.1)
        ]

wallpaper :: RealFloat a => Wallpaper a -> IO ()
wallpaper wp = symmetryPattern (wpOptions wp)
                               (recipe (wpGroup wp))
                               (wpCoefs wp)
                               (wpType wp)
                               (wpProcess wp)
                               (wpWheel wp)
                               (wpPath wp)
