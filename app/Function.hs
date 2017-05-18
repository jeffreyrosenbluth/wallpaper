
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Portrait
import           Recipes.Functions
import           Types

import           Data.Yaml          (ParseException, decodeFileEither)
import           System.Environment

main :: IO ()
main = do
  [yamlFile] <- getArgs
  (wp :: Either ParseException (Wallpaper Double)) <- decodeFileEither yamlFile
  case wp of
    Left e   -> error (show e)
    Right w  -> standardPortrait w

idPortrait :: RealFloat a => Wallpaper a -> IO ()
idPortrait wp = phasePortrait (wpOptions wp) identity (wpPath wp)

standardPortrait :: RealFloat a => Wallpaper a -> IO ()
standardPortrait wp = phasePortrait (wpOptions wp) standard (wpPath wp)
