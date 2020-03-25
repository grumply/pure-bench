{-# language GeneralizedNewtypeDeriving, DeriveGeneric, ScopedTypeVariables #-}
module Pure.Bench.Space where

import Pure.Data.JSON
import Pure.Test.Pretty
import Pure.Variance
import Pure.Spacetime

import GHC.Generics

newtype ByteUsageSamples = ByteUsageSamples { getByteUsageSamples :: Count }
  deriving (Generic,Eq,Ord,Num,Real,Read,Show,Floating,Fractional,RealFrac,Base,Similar,ToJSON,FromJSON)

instance IsCount ByteUsageSamples where
  toCount = ByteUsageSamples
  fromCount (ByteUsageSamples bus) = bus

newtype Mutated = Mutated { getMutated :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Mutated

instance IsSpace Mutated where
  toSpace = Mutated
  fromSpace = getMutated

instance Similar Mutated where
  sim b (Megabytes mb1) (Megabytes mb2) = sim b mb1 mb2

instance Improving Mutated where
  improving b1 b2 = b1 > b2 -- As a default, fewer mutated bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype Allocated = Allocated { getAllocated :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Allocated

instance IsSpace Allocated where
  toSpace = Allocated
  fromSpace = getAllocated

instance Similar Allocated where
  sim b (Megabytes mb1) (Megabytes mb2) = sim b mb1 mb2

instance Improving Allocated where
  improving b1 b2 = b1 > b2 -- As a default, fewer allocated bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype Deallocated = Deallocated { getDeallocated :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Deallocated

instance IsSpace Deallocated where
  toSpace = Deallocated
  fromSpace = getDeallocated

instance Similar Deallocated where
  sim b (Megabytes mb1) (Megabytes mb2) = sim b mb1 mb2

instance Improving Deallocated where
  improving b1 b2 = b1 > b2 -- As a default, fewer deallocated bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype Slop = Slop { getSlop :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Slop

instance IsSpace Slop where
  toSpace = Slop
  fromSpace = getSlop

instance Similar Slop where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Slop where
  improving b1 b2 = b1 > b2 -- As a default, fewer slop bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype Max = Max { getMax :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Max

instance IsSpace Max where
  toSpace = Max
  fromSpace = getMax

instance Similar Max where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Max where
  improving b1 b2 = b1 > b2 -- As a default, fewer maximum bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype Cumulative = Cumulative { getCumulative :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Cumulative

instance IsSpace Cumulative where
  toSpace = Cumulative
  fromSpace = getCumulative

instance Similar Cumulative where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Cumulative where
  improving b1 b2 = b1 > b2 -- As a default, fewer cumulative bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype Copied = Copied { getCopied :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Copied

instance IsSpace Copied where
  toSpace = Copied
  fromSpace = getCopied

instance Similar Copied where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Copied where
  improving b1 b2 = b1 > b2 -- As a default, fewer copied bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype Used = Used { getUsed :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Magnitude,Base)

instance Vary Used

instance IsSpace Used where
  toSpace = Used
  fromSpace = getUsed

instance Similar Used where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Used where
  improving b1 b2 = b1 > b2 -- As a default, fewer used bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype MaxSlop = MaxSlop { getMaxSlop :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary MaxSlop

instance IsSpace MaxSlop where
  toSpace = MaxSlop
  fromSpace = getMaxSlop

instance Similar MaxSlop where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving MaxSlop where
  improving b1 b2 = b1 > b2 -- As a default, fewer maximum slop bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype Peak = Peak { getPeak :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Peak

instance IsSpace Peak where
  toSpace = Peak
  fromSpace = getPeak

instance Similar Peak where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Peak where
  improving b1 b2 = b1 > b2 -- As a default, fewer peak bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype Live = Live { getLive :: SpaceInBytes }
  deriving (Generic,Eq,Ord,Num,Real,RealFrac,Floating,RealFloat,Fractional,Read,Show,Pretty,ToJSON,FromJSON,Base,Magnitude)

instance Vary Live

instance IsSpace Live where
  toSpace = Live
  fromSpace = getLive

instance Similar Live where
  sim b (Bytes b1) (Bytes b2) = sim b b1 b2

instance Improving Live where
  improving b1 b2 = b1 > b2 -- As a default, fewer live bytes are better. This is not an absolute truth!
  improvingShow _ = ">"

newtype CopyRate = CopyRate { getCopyRate :: BytesPerSecond }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,Pretty,ToJSON,FromJSON)

instance Vary CopyRate

instance IsDataRate CopyRate where
  toDataRate = CopyRate
  fromDataRate = getCopyRate

instance Improving CopyRate -- more throughput is better

instance Magnitude CopyRate

instance Base CopyRate where
  base _ = 2

instance Similar CopyRate where
  sim b (DataRate (Megabytes mbps :: SpaceInBytes) (_ :: Seconds))
            (DataRate (Megabytes mbps':: SpaceInBytes) (_ :: Seconds))
    = sim b mbps mbps'

newtype AllocationRate = AllocationRate { getAllocationRate :: BytesPerSecond }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,Pretty,ToJSON,FromJSON)

instance Vary AllocationRate

instance Magnitude AllocationRate

instance IsDataRate AllocationRate where
  toDataRate = AllocationRate
  fromDataRate = getAllocationRate

instance Improving AllocationRate -- more throughput is better

instance Base AllocationRate where
  base _ = 2

instance Similar AllocationRate where
  sim b
    (DataRate (Megabytes mbps :: SpaceInBytes) (_ :: Seconds))
    (DataRate (Megabytes mbps' :: SpaceInBytes) (_ :: Seconds))
      = sim b mbps mbps'

newtype DeallocationRate = DeallocationRate { getDeallocationRate :: BytesPerSecond }
  deriving (Generic,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,RealFloat,Read,Show,Pretty,ToJSON,FromJSON)

instance Vary DeallocationRate

instance Magnitude DeallocationRate

instance IsDataRate DeallocationRate where
  toDataRate = DeallocationRate
  fromDataRate = getDeallocationRate

instance Improving DeallocationRate -- more throughput is better

instance Base DeallocationRate where
  base _ = 2

instance Similar DeallocationRate where
  sim b
    (DataRate (Megabytes mbps :: SpaceInBytes) (_ :: Seconds))
    (DataRate (Megabytes mbps' :: SpaceInBytes) (_ :: Seconds))
      = sim b mbps mbps'
