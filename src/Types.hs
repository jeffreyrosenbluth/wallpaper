{-# LANGUAGE StrictData            #-}
{-# LANGUage TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Types where

import           Codec.Picture
import           Data.Complex

type Recipe a = Complex a -> Complex a

data Coef a = Coef
  { nCoord :: Int
  , mCoord :: Int
  , anm    :: Complex a
  } deriving (Show, Eq)

data Options a = Options
  { width  :: Int
  , height :: Int
  , scale  :: a
  , focus  :: a
  }

defaultOpts :: RealFloat a => Options a
defaultOpts = Options 750 750 0.5 0.5

class Img a where
  type Pxl a :: *
  getPxl :: a -> Int -> Int-> Pxl a
  generateImg :: (Int -> Int -> Pxl a) -> Int -> Int -> a
  imgWidth :: a -> Int
  imgHeight :: a -> Int

instance Pixel p => Img (Image p) where
  type Pxl (Image p) = p
  getPxl = pixelAt
  generateImg = generateImage
  imgWidth = imageWidth
  imgHeight = imageHeight

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
