{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Core
import           Types
import           Juicy
import           Recipe

import           Data.Complex
import           Data.Yaml          (decodeFileEither, ParseException)
import           Codec.Picture
import           System.Environment

main :: IO ()
main = do
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
