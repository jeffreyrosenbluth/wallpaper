{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Portrait
import           Types

import           Data.Complex
import           Data.Yaml          (ParseException, decodeFileEither)
import           System.Environment

main :: IO ()
main = do
  [yamlFile] <- getArgs
  (rs :: Either ParseException (Rosette Double)) <- decodeFileEither yamlFile
  case rs of
    Left e   -> error (show e)
    Right r  -> rosette' r

coefs :: [Coef Double]
coefs = [ Coef 1 0 (0.75:+0.25)
        , Coef (-2) 2 (0.2:+(-0.2))
        , Coef 1 (-1) (0.6:+0.1)
        ]

rosette' :: RealFloat a => Rosette a -> IO ()
rosette' rs = rosette (rsOptions rs)
                     (rsCoefs rs)
                     (rsFoldSym rs)
                     (rsMirror rs)
                     (rsProcess rs)
                     (rsWheel rs)
                     (rsPath rs)
