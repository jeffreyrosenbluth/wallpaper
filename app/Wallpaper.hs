{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Portrait
import           Types

import           Data.Yaml          (ParseException, decodeFileEither)
import           System.Environment

main :: IO ()
main = do
  [yamlFile] <- getArgs
  (wp :: Either ParseException (Wallpaper Double)) <- decodeFileEither yamlFile
  case wp of
    Left e   -> error (show e)
    Right w  -> wallpaper w

wallpaper :: RealFloat a => Wallpaper a -> IO ()
wallpaper wp = symmetryPattern (wpOptions wp)
                               (recipe (wpGroup wp))
                               (wpCoefs wp)
                               (wpType wp)
                               (wpProcess wp)
                               (wpWheel wp)
                               (wpPath wp)

phase:: RealFloat a => Wallpaper a -> IO ()
phase wp = symmetryPortrait (wpOptions wp)
                            (recipe (wpGroup wp))
                            (wpCoefs wp)
                            (wpPath wp)
