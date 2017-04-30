module Recipes.Common where

import           Recipes.Frieze
import           Recipes.Wallpaper
import           Types

-- Recip Builder ---------------------------------------------------------------

recipe :: RealFloat a => SymmetryGroup a -> [Coef a] -> Recipe a
recipe sg = case sg of
  P1 a b -> p1 a b
  P2 a b -> p2 a b
  CM a   -> cm a
  CMM a  -> cmm a
  PM a   -> pm a
  PG a   -> pg a
  PMM a  -> pmm a
  PMG a  -> pmg a
  PGG a  -> pgg a
  P4     -> p4
  P4M    -> p4m
  P4G    -> p4g
  P3     -> p3
  P31M   -> p31m
  P3M1   -> p3m1
  P6     -> p6
  P6M    -> p6m
  P111   -> p111
  P211   -> p211
  P1M1   -> p1m1
  P11M   -> p11m
  P11G   -> p11g
  P2MM   -> p2mm
  P2MG   -> p2mg
