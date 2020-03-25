{-# language DeriveGeneric, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Pure.Bench.Time where

import Pure.Data.JSON
import Pure.Variance
import Pure.Spacetime
import Pure.Test

import GHC.Generics

newtype Elapsed = Elapsed Seconds
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON,Pretty,Magnitude)

instance Vary Elapsed

instance IsTime Elapsed where
  toTime = Elapsed
  fromTime (Elapsed e) = e

instance Improving Elapsed where
  improving old new = old > new -- less time is better
  improvingShow _ = ">"

instance Base Elapsed where
  base (Seconds i) -- yeah, probably not a good idea -- I'm hoping it's intuitive
    | i > 1     = 60
    | otherwise = 10

instance Similar Elapsed where
  sim b (Nanoseconds d) (Nanoseconds d') = sim b d d'

class HasElapsed a where
  elapsed :: a -> Elapsed

newtype CPUTime = CPUTime Seconds
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON,Pretty,Magnitude)

instance Vary CPUTime

instance IsTime CPUTime where
  toTime = CPUTime
  fromTime (CPUTime cput) = cput

instance Improving CPUTime where
  improving old new = old > new
  improvingShow _ = ">"

instance Base CPUTime

instance Similar CPUTime where
  sim b (Nanoseconds us) (Nanoseconds us') = sim b us us'

class HasCPUTime a where
  cpuTime :: a -> CPUTime
