{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StrictData           #-}

module Types where

import           Codec.Picture
import           Data.Complex

type Recipe a = Complex a -> Complex a

data Coef a = Coef
  { nCoord :: Int
  , mCoord :: Int
  , anm    :: Complex a
  } deriving (Show, Eq)

data Frieze
  = P111
  | P211
  | P1M1
  | P11M
  | P11G
  | P2MM
  | P2MG

data Wallpaper a
  = P1 a a
  | P2 a a
  | CM a
  | CMM a
  | PM a
  | PG a
  | PMM a
  | PMG a
  | PGG a
  | P4
  | P4M
  | P4G
  | P3
  | P31M
  | P3M1
  | P6
  | P6M

data Rosette

data SymmetryType a = W (Wallpaper a) | F Frieze | R Rosette

data Options a = Options
  { width    :: Int
  , height   :: Int
  , scale    :: a
  , focus    :: a
  , coefs    :: [Coef a]
  , symmetry :: SymmetryType a
  }

class BlackWhite a where
  black :: a
  white :: a

instance BlackWhite PixelRGBA8 where
  black = PixelRGBA8 0 0 0 255
  white = PixelRGBA8 255 255 255 255

instance BlackWhite PixelRGB8 where
  black = PixelRGB8 0 0 0
  white = PixelRGB8 255 255 255

instance BlackWhite PixelYCbCr8 where
  black = PixelYCbCr8 0 0 0
  white = PixelYCbCr8 255 255 255

instance BlackWhite Pixel8 where
  black = 0
  white = 255

instance BlackWhite PixelYA8 where
  black = PixelYA8 0 255
  white = PixelYA8 255 255

instance BlackWhite PixelCMYK8 where
  black = PixelCMYK8  0 0 0 255
  white = PixelCMYK8 0 0 0 0
