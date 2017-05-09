{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Juicy
import           Recipes.Functions
import           Types

import           Data.Complex
import           Data.Yaml          (ParseException, decodeFileEither)
import           System.Environment

main :: IO ()
main = do
  [yamlFile] <- getArgs
  (wp :: Either ParseException (Wallpaper Double)) <- decodeFileEither yamlFile
  case wp of
    Left e   -> error (show e)
    Right w  -> standardPortrait w -- wallpaper w

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

phasePortrait :: RealFloat a => Wallpaper a -> IO ()
phasePortrait wp = symmetryPortrait (wpOptions wp)
                                    (recipe (wpGroup wp))
                                    (wpCoefs wp)
                                    (wpPath wp)

idPortrait :: RealFloat a => Wallpaper a -> IO ()
idPortrait wp = portrait (wpOptions wp) identity (wpPath wp)

standardPortrait :: RealFloat a => Wallpaper a -> IO ()
standardPortrait wp = portrait (wpOptions wp) standard (wpPath wp)
