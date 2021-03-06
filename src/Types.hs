{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies         #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (c) 2017 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- Types and classes for creating symmtery images using the domain coloring
-- algortihm.
--
-- <<examples/morph.png>>
--------------------------------------------------------------------------------

module Types
  ( ColorSource(..)
  , Coef(..)
  , Options(..)
  , defaultOpts
  , SymmetryGroup(..)
  , PreProcess(..)
  , WPtype(..)
  , Wallpaper(..)
  , Rosette(..)
  , Function(..)
  , Recipe
  , Invertible(..)
  , BlackWhite(..)
  ) where

import           Complextra

import           Codec.Picture
import           Data.Text     (Text, toLower)
import           Data.Yaml

-- | A 'Recipe' is a mapping from the Complex plange to the Complex plane.
type Recipe = Complex -> Complex

-- | A color source can be either a JuicyPixels image or a function from
--   a Complex number to a pixel.
data ColorSource p
  = Picture (Image p)
  | Function (Complex -> p)

-- | The coefficents used to build a symmetry recipe, C_nm. A coeffient
--   is a doubley indexed Complex number
data Coef = Coef
  { nCoord :: Int     -- ^ The first index.
  , mCoord :: Int     -- ^ The second index.
  , anm    :: Complex -- ^ The coefficient.
  } deriving (Show, Eq)

instance FromJSON Complex where
  parseJSON a@(Array _) = do
    (m, d) <- parseJSON a
    return $ mkPolar m (pi * d / 180)
  parseJSON _ = fail "Expected Array for a Complex value."

instance FromJSON Coef where
  parseJSON (Object v)
    =   Coef
    <$> v .: "n"
    <*> v .: "m"
    <*> v .: "A(n,m)"
  parseJSON _ = fail "Expected Object for Coef value."

-- | Settings for the size, repeat lenght, and scaling factor for creating a
--   a domain coloring.
data Options = Options
  { width     :: Int -- ^ The width of the created image.
  , height    :: Int -- ^ The height of the created iamge.
  , repLength :: Int -- ^ The length of the pattern to repeat.
  , origin    :: (Double, Double)
  , scale     :: Double   -- ^ Usually set less than 1, to Complexensate for the
                     --   fact that the color wheel is not infinite.
  , rotation  :: Double
  , morphing  :: Bool
  } deriving (Show, Eq)

instance FromJSON Options where
  parseJSON (Object v)
    =   Options
    <$> v .: "width"
    <*> v .: "height"
    <*> v .: "repeat-length"
    <*> v .: "origin"
    <*> v .: "scale-factor"
    <*> v .: "rotation"
    <*> v .: "morphing"
  parseJSON _ = fail "Expected Object for a Options value."

-- | The defaul 'Options' creates a square 750 x 750 pixel image,
--   with a repeat of 150 pixels and scales the pixel lookup coordintes
--   by 1/2.
defaultOpts :: Options
defaultOpts = Options 750 750 150 (0, 0) 0.5 0 False

-- | The 17 Wallpaper groups and 7 Frieze groups.
data SymmetryGroup
  = P1 Double Double -- ^ Arguments are &#x3BE; and &#x3B7;.
           --    The lattice vectors are 1 and &#x3BE; + /i/ &#x3B7;.
  | P2 Double Double -- ^ Arguments are &#x3BE; and &#x3B7;.
           --   The lattice vectors are 1 and &#x3BE; + /i/ &#x3B7;.
  | CM Double   -- ^ The argument is /b/ with lattice vectors 1//2 +- ib/.
  | CMM Double  -- ^ The argument is /b/ with lattice vectors 1//2 +- ib/.
  | PM Double   -- ^ The argument is /L/ with lattice vectors 1 and /iL/.
  | PG Double   -- ^ The argument is /L/ with lattice vectors 1 and /iL/.
  | PMM Double  -- ^ The argument is /L/ with lattice vectors 1 and /iL/.
  | PMG Double  -- ^ The argument is /L/ with lattice vectors 1 and /iL/.
  | PGG Double  -- ^ The argument is /L/ with lattice vectors 1 and /iL/.
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
  deriving (Show, Eq)

instance FromJSON SymmetryGroup where
  parseJSON (Object v) = do
    (name :: Text) <- v .: "name"
    case toLower name of
      "p1"  -> P1  <$> v .: "xi" <*> v .: "eta"
      "p2"  -> P2  <$> v .: "xi" <*> v .: "eta"
      "cm"  -> CM  <$> v .: "b"
      "cmm" -> CMM <$> v .: "b"
      "pm"  -> PM  <$> v .: "L"
      "pg"  -> PG  <$> v .: "L"
      "pmm" -> PMM <$> v .: "L"
      "pmg" -> PMG <$> v .: "L"
      "pgg" -> PGG <$> v .: "L"
      s     -> parseGroup s
  parseJSON (String s) = parseGroup s
  parseJSON _ = fail "Group must be an object or String"

parseGroup :: Monad m => Text -> m SymmetryGroup
parseGroup s = case toLower s of
  "p4"   -> pure P4
  "p4m"  -> pure P4M
  "p4g"  -> pure P4G
  "p3"   -> pure P3
  "p31m" -> pure P31M
  "p3m1" -> pure P3M1
  "p6"   -> pure P6
  "p6m"  -> pure P6M
  "p111" -> pure P111
  "p211" -> pure P211
  "p1m1" -> pure P1M1
  "p11m" -> pure P11M
  "p11g" -> pure P11G
  "p2mm" -> pure P2MM
  "p2mg" -> pure P2MG
  _      -> fail "Tried to parse an invalid group name."

-- | The type of wallpaper to produce.
data WPtype
  = Plain
  | Morph Double
  | Blend SymmetryGroup
  deriving (Show, Eq)

instance FromJSON WPtype where
  parseJSON (Object v) = do
    (typ :: String) <- v .: "style"
    case typ of
      "plain" -> pure Plain
      "morph" -> Morph <$> v .: "cutoff"
      "blend" -> Blend <$> v .: "group"
      _       -> fail "Tried to parse an invalide wallpaper type."
  parseJSON _ = fail "Expected a String for the wallpaper type."

-- | What to do the color wheel before creating the Wallpaper.
data PreProcess
  = FlipHorizontal
  | FlipVertical
  | FlipBoth
  | Invert
  | AntiSymmHorizontal
  | AntiSymmVertical
  | None
  deriving (Show, Eq)

instance FromJSON PreProcess where
  parseJSON (String s) =
    case toLower s of
      "fliphorizontal"     -> pure FlipHorizontal
      "flipvertical"       -> pure FlipVertical
      "flipboth"           -> pure FlipBoth
      "invert"             -> pure Invert
      "antisymmvertical"   -> pure AntiSymmVertical
      "antisymmhorizontal" -> pure AntiSymmHorizontal
      "none"               -> pure None
      _                    -> fail "Invalid Pre-process type"
  parseJSON _ = fail "Pre-process must be a String"

-- | Settings for creating a wallpaper.
data Wallpaper = Wallpaper
  { wpGroup   :: SymmetryGroup
  , wpCoefs   :: [Coef]
  , wpType    :: WPtype
  , wpOptions :: Options
  , wpWheel   :: Maybe FilePath
  , wpProcess :: PreProcess
  , wpPath    :: FilePath
  } deriving (Show, Eq)

instance FromJSON Wallpaper where
  parseJSON (Object v)
    =   Wallpaper
    <$> v .: "Group"
    <*> v .: "Coefficients"
    <*> v .:? "Type" .!= Plain
    <*> v .: "Options"
    <*> v .:? "Colorwheel-path" .!= Nothing
    <*> v .:? "Pre-process" .!= None
    <*> v .: "Output-path"
  parseJSON _ = fail "Expected Object for Wallpaper value."

-- | Settings for creating a rosette.
data Rosette = Rosette
  { rsFoldSym :: Int
  , rsMirror  :: Bool
  , rsCoefs   :: [Coef]
  , rsOptions :: Options
  , rsWheel   :: Maybe FilePath
  , rsProcess :: PreProcess
  , rsPath    :: FilePath
  } deriving (Show, Eq)

instance FromJSON Rosette where
  parseJSON (Object v)
    =   Rosette
    <$> v .: "P-fold"
    <*> v .: "Mirror"
    <*> v .: "Coefficients"
    <*> v .: "Options"
    <*> v .:? "Colorwheel-path" .!= Nothing
    <*> v .:? "Pre-process" .!= None
    <*> v .: "Output-path"
  parseJSON _ = fail "Expected Object for Rosette value."

-- | Settings for creating a phase portrait.
data Function = Fn
  { fnOptions :: Options
  , fnWeel    :: FilePath
  , fnProcess :: PreProcess
  , fnPath    :: FilePath
  } deriving (Show, Eq)


--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
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
