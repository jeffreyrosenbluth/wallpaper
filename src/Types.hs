{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUage TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

---------------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Types and classes for creating symmtery images using the domain coloring
-- algortihm.
---------------------------------------------------------------------------------

module Types
  ( Coef(..)
  , Options(..)
  , defaultOpts
  , SymmetryGroup(..)
  , Wallpaper(..)
  , Recipe
  , Img(..)
  , Invertible(..)
  , BlackWhite(..)
  ) where

import           Codec.Picture
import           Data.Complex (Complex)

-- | A 'Recipe' is a mapping from the complex plange to the complex plane.
type Recipe a = Complex a -> Complex a

-- | The coefficents used to build a symmetry recipe, C_nm. A coeffient
--   is a doubley indexed complex number
data Coef a = Coef
  { nCoord :: Int       -- ^ The first index.
  , mCoord :: Int       -- ^ The second index.
  , anm    :: Complex a -- ^ The coefficient.
  } deriving (Show, Eq, Functor)

-- | Settings for creating a symmetry image.
data Options a = Options
  { width     :: Int -- ^ The width of the created image.
  , height    :: Int -- ^ The height of the created iamge.
  , repLength :: Int -- ^ The length of the pattern to repeat.
  , scale     :: a   -- ^ Usually set less than 1, to compensate for the
                     --   fact that the color wheel is not infinite.
  }

-- | The defaul 'Options' creates a square 750 x 750 pixel image,
--   with a repeat of 150 pixels and scales the pixel lookup coordintes
--   by 1/2.
defaultOpts :: Options Double
defaultOpts = Options 750 750 150 0.5

-- | Things that symmetry images and be output as.
class Img a where
  type Pxl a  :: *
  getPxl      :: a -> Int -> Int-> Pxl a
  generateImg :: (Int -> Int -> Pxl a) -> Int -> Int -> a
  imgWidth    :: a -> Int
  imgHeight   :: a -> Int

-- | 'Img' instance for a JuicyPixels 'Image'.
instance Pixel p => Img (Image p) where
  type Pxl (Image p) = p
  getPxl      = pixelAt
  generateImg = generateImage
  imgWidth = imageWidth
  imgHeight = imageHeight

-- | The 17 Wallpaper groups and 7 Frieze groups.
data SymmetryGroup a
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
  | P111
  | P211
  | P1M1
  | P11M
  | P11G
  | P2MM
  | P2MG
  deriving (Show, Eq, Functor)

data Wallpaper a = Wallpaper
  { wpOptions :: Options a
  , wpGroup   :: SymmetryGroup a
  , wpCoefs   :: [Coef a]}

---------------------------------------------------------------------------------

-- | Pixels that can be set to black and white.
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

---------------------------------------------------------------------------------

-- | Invert the color of a pixel.
class Invertible a where
  invert :: a -> a

instance Invertible PixelRGBA8 where
  invert (PixelRGBA8 r g b a) = PixelRGBA8 (255-r) (255-g) (255-b) a

instance Invertible PixelRGB8 where
  invert (PixelRGB8 r g b)  = PixelRGB8 (255-r) (255-g) (255-b)

instance Invertible PixelYCbCr8 where
  invert (PixelYCbCr8 r g b)  = PixelYCbCr8 (255-r) (255-g) (255-b)

instance Invertible Pixel8 where
  invert p = 255 - p

instance Invertible PixelYA8 where
  invert (PixelYA8 c a) = PixelYA8 (255-c) a

instance Invertible PixelCMYK8 where
  invert (PixelCMYK8 r g b a) = PixelCMYK8 (255-r) (255-g) (255-b) a
