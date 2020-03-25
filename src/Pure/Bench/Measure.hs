{-# language ExistentialQuantification #-}
module Pure.Bench.Measure where

import Pure.Spacetime
import Pure.Test.Pretty

data Measure b
  = forall a. (Magnitude a, Base a, Improving a, Similar a, Pretty a) => Measure (b -> a)


