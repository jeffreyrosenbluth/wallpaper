module Recipe where

import           Types
import           Complextra

import           Data.Complex

-- | Center the coordinates at the origin and scale them to the range (-1, 1)
focusIn :: RealFloat a => Int -> Int -> Int -> Recipe a -> Recipe a
focusIn w h l rcp (x :+ y) =
  rcp ((x - fromIntegral w / 2) / l' :+ (fromIntegral h / 2 - y) / l')
    where
      l' = fromIntegral l

wallpaper :: RealFloat a => (Int -> Int -> Recipe a) -> [Coef a] -> Recipe a
wallpaper mkRecipe cs z = sum $ zipWith (*) as rs
  where
    as = anm <$> cs
    rs = ($ z) . uncurry mkRecipe <$> [(nCoord c, mCoord c) | c <- cs]

negateCnm :: Coef a -> Coef a
negateCnm (Coef n m a) = Coef (-n) (-m) a

negateCm :: Coef a -> Coef a
negateCm (Coef n m a) = Coef n (-m) a

reverseCnm :: Coef a -> Coef a
reverseCnm (Coef n m a) = Coef m n a

-- | Does not commute with negate or reverse, usuall you want to apply
--   'alternateCanm' first.
alternateCanm :: RealFloat a => (Int -> Int -> Int) -> Coef a -> Coef a
alternateCanm alt (Coef n m a) = Coef n m (fromIntegral (alt n m) .*^ a)
