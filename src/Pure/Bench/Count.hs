{-# language GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Pure.Bench.Count where

import Pure.Data.JSON hiding (pretty)
import Pure.Test.Pretty
import Pure.Spacetime.Frequency
import Pure.Variance
import Pure.Spacetime

import GHC.Generics
import Text.Printf

newtype Collections = Collections { getCollections :: Count }
  deriving (Generic,Eq,Ord,Num,Real,Floating,Fractional,RealFrac,Read,Show,ToJSON,FromJSON,Base,Magnitude,Similar)

instance Vary Collections

instance IsCount Collections where
  toCount = Collections
  fromCount = getCollections

instance Pretty Collections where
  pretty (Collections c) = pretty c

instance Improving Collections where
  improving c1 c2 = c1 > c2
  improvingShow _ = ">"

newtype CollectionFrequency = CollectionFrequency { getCollectionFrequency :: Hertz }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,ToJSON,FromJSON)

instance Vary CollectionFrequency
instance Similar CollectionFrequency
instance Magnitude CollectionFrequency

instance IsFrequency CollectionFrequency where
  toFrequency = CollectionFrequency
  fromFrequency (CollectionFrequency ps) = ps

instance Improving CollectionFrequency where
  improving = (>)
  improvingShow _ = ">"

instance Base CollectionFrequency

instance Pretty CollectionFrequency where
  pretty (CollectionFrequency (Hertz c)) = printf "%d collections/s" c
