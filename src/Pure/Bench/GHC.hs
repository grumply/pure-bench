{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Pure.Bench.GHC (module Pure.Bench.GHC, module Export) where

import Pure.Bench.Count
import Pure.Bench.Measure
import Pure.Bench.Space
import Pure.Bench.Time

import Pure.Test as Export
import Pure.Spacetime
import Pure.Data.JSON hiding (pretty)
import Pure.Variance

import Data.Int
import Data.List (intercalate)

import Control.DeepSeq
import Control.Exception
import Data.Monoid
import Data.Semigroup
import GHC.Exts
import GHC.Generics
import GHC.Stats hiding (gc)
import qualified GHC.Stats as GHC
import System.Mem
import System.IO.Unsafe

{-# INLINE mkRuntimeStats #-}
mkRuntimeStats :: RTSStats -> RTSStats -> RuntimeStats
mkRuntimeStats before after =
    let !rs_cpuElapsed  = Nanoseconds (realToFrac $ elapsed_ns after)         - Nanoseconds (realToFrac $ elapsed_ns before)
        !rs_cputime     = Nanoseconds (realToFrac $ cpu_ns after)             - Nanoseconds (realToFrac $ cpu_ns before)
        !rs_mutElapsed  = Nanoseconds (realToFrac $ mutator_elapsed_ns after) - Nanoseconds (realToFrac $ mutator_elapsed_ns before)
        !rs_mutTime     = Nanoseconds (realToFrac $ mutator_cpu_ns after)     - Nanoseconds (realToFrac $ mutator_cpu_ns before)
        !rs_gcElapsed   = Nanoseconds (realToFrac $ gc_elapsed_ns after)      - Nanoseconds (realToFrac $ gc_elapsed_ns before)
        !rs_gcTime      = Nanoseconds (realToFrac $ gc_cpu_ns after)          - Nanoseconds (realToFrac $ gc_cpu_ns before)
        !rs_allocated   = Bytes (realToFrac $ allocated_bytes after)          - Bytes (realToFrac $ allocated_bytes before)  - rtsStats_size
        !rs_uncollected = Bytes (realToFrac $ max_live_bytes after)           - Bytes (realToFrac $ max_live_bytes before)
        !rs_copied      = Bytes (realToFrac $ copied_bytes after)             - Bytes (realToFrac $ copied_bytes before)
        !rs_slop        = Bytes (realToFrac $ gcdetails_slop_bytes gcAfter)   - Bytes (realToFrac $ gcdetails_slop_bytes gcBefore)
        !rs_collections = Count (realToFrac $ gcs after)                      - Count (realToFrac $ gcs before)
    in RuntimeStats {..}
  where
    rtsStats_size = Bytes 320
    gcBefore = GHC.gc before
    gcAfter  = GHC.gc after

data RuntimeStats = RuntimeStats
    { rs_cpuElapsed   :: {-# UNPACK #-}!Elapsed
    , rs_cputime      :: {-# UNPACK #-}!CPUTime
    , rs_mutElapsed   :: {-# UNPACK #-}!Elapsed
    , rs_mutTime      :: {-# UNPACK #-}!CPUTime
    , rs_allocated    :: {-# UNPACK #-}!Allocated
    -- , mutated      :: {-# UNPACK #-}!Mutated
    , rs_gcElapsed    :: {-# UNPACK #-}!Elapsed
    , rs_gcTime       :: {-# UNPACK #-}!CPUTime
    , rs_collections  :: {-# UNPACK #-}!Collections
    , rs_uncollected  :: {-# UNPACK #-}!Live
    , rs_copied       :: {-# UNPACK #-}!Copied
    , rs_slop         :: {-# UNPACK #-}!Slop
    } deriving (Generic, Read, Show, Eq, ToJSON, FromJSON)

instance Vary RuntimeStats
instance Similar RuntimeStats
instance Magnitude RuntimeStats

type Benchmark = Test Sync ()

withEnv :: (NFData env) => Test Sync env -> (env -> Benchmark) -> Benchmark
withEnv mkenv f = do
  e <- mkenv
  io $ evaluate $ rnf e
  f e

withEnvCleanup :: (NFData env) => Test Sync env -> (env -> Benchmark) -> (env -> Benchmark) -> Benchmark
withEnvCleanup mkenv f c = withEnv mkenv (\env -> f env >> c env)

data BenchResult = BenchResult
  { br_runs :: {-# UNPACK #-}!Count
  , br_dur :: {-# UNPACK #-}!CPUTime
  , br_cpu :: {-# UNPACK #-}!CPUTime
  , br_mut :: {-# UNPACK #-}!CPUTime
  , br_gc  :: {-# UNPACK #-}!CPUTime
  , br_alloc :: {-# UNPACK #-}!Allocated
  , br_copy :: {-# UNPACK #-}!Copied
  , br_coll :: {-# UNPACK #-}!Collections
  } deriving (Show,Read,Eq,Generic,ToJSON,FromJSON)

instance Vary BenchResult
instance Similar BenchResult
instance Magnitude BenchResult

instance Monoid BenchResult where
  {-# INLINE mempty #-}
  mempty = BenchResult 0 0 0 0 0 0 0 0
  {-# INLINE mappend #-}
  mappend = (<>)

instance Semigroup BenchResult where
  {-# INLINE (<>) #-}
  (<>) br1 br2 =
    let !brruns  = br_runs br1  + br_runs br2
        !brdur   = br_dur br1   + br_dur br2
        !brcpu   = br_cpu br1   + br_cpu br2
        !brmut   = br_mut br1   + br_mut br2
        !brgc    = br_gc br1    + br_gc br2
        !bralloc = br_alloc br1 + br_alloc br2
        !brcopy  = br_copy br1  + br_copy br2
        !brcoll  = br_coll br1  + br_coll br2
    in BenchResult brruns brdur brcpu brmut brgc bralloc brcopy brcoll

{-# INLINE allocRate #-}
allocRate :: BenchResult -> AllocationRate
allocRate br = DataRate (alloc br) (mut br) -- allocation per MUT second, like -RTS +s

{-# INLINE copyRate #-}
copyRate :: BenchResult -> CopyRate
copyRate br = DataRate (copy br) (gc br)

{-# INLINE runs #-}
runs :: BenchResult -> Count
runs = br_runs

{-# INLINE dur #-}
dur :: BenchResult -> CPUTime
dur = br_dur

{-# INLINE cpu #-}
cpu :: BenchResult -> CPUTime
cpu BenchResult {..} = br_cpu / realToFrac br_runs

{-# INLINE mut #-}
mut :: BenchResult -> CPUTime
mut BenchResult {..} = br_mut / realToFrac br_runs

{-# INLINE gc #-}
gc :: BenchResult -> CPUTime
gc BenchResult {..} = br_gc / realToFrac br_runs

{-# INLINE alloc #-}
alloc :: BenchResult -> Allocated
alloc BenchResult {..} = br_alloc / realToFrac br_runs

{-# INLINE copy #-}
copy :: BenchResult -> Copied
copy BenchResult {..} = br_copy / realToFrac br_runs

{-# INLINE coll #-}
coll :: BenchResult -> Collections
coll BenchResult {..} = br_coll

instance Pretty BenchResult where
    pretty br@BenchResult {..} =
        unlines
            [ ""
            , divider
            , header2
            , divider
            , cpuTimeStats
            , mutTimeStats
            , gcTimeStats
            , ""
            , "Runs:       " <> pad 11 (pretty (runs br))
            , "Duration:   " <> pad 11 (pretty (dur br))
            , "Collections:" <> pad 11 (pretty (coll br))
            ]
      where
        header2 = "                Time           |       Space |    Throughput"
        divider = "     -------------------------------------------------------"

        cpuTimeStats =
          "CPU:"    <> p (cpu br)

        mutTimeStats =
          "MUT:"    <> p (mut br)
            <> "  " <> p (mkPercent (mut br) (cpu br))
            <> "  " <> p (alloc br)
            <> "  " <> p (allocRate br)

        gcTimeStats =
          "GC: "    <> p (gc br)
            <> "  " <> p (mkPercent (gc br) (cpu br))
            <> "  " <> p (copy br)
            <> "  " <> p (copyRate br)

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s

data BenchDiff = BenchDiff
  { bd_bench1 :: {-# UNPACK #-}!BenchResult
  , bd_bench2 :: {-# UNPACK #-}!BenchResult
  , bd_cpu    :: {-# UNPACK #-}!Multiplier
  , bd_mut    :: {-# UNPACK #-}!Multiplier
  , bd_gc     :: {-# UNPACK #-}!Multiplier
  , bd_alloc  :: {-# UNPACK #-}!Multiplier
  , bd_copy   :: {-# UNPACK #-}!Multiplier
  , bd_coll   :: {-# UNPACK #-}!Multiplier
  } deriving (Read,Show,Eq,Generic,ToJSON,FromJSON)

{-# INLINE diff #-}
diff :: BenchResult -> BenchResult -> BenchDiff
diff br1 br2 = BenchDiff {..}
  where
    -- little over-judicious with the realToFrac calls here; try to reduce
    bd_bench1 = br1
    bd_bench2 = br2
    bd_cpu    = realToFrac (br_cpu br2 / realToFrac (br_runs br2)) / realToFrac (br_cpu br1 / realToFrac (br_runs br1))
    bd_mut    = realToFrac (br_mut br2 / realToFrac (br_runs br2)) / realToFrac (br_mut br1 / realToFrac (br_runs br1))
    bd_gc     = realToFrac (br_gc br2 / realToFrac (br_runs br2)) / realToFrac (br_gc br1 / realToFrac (br_runs br1))
    bd_alloc  = realToFrac (realToFrac (br_alloc br2) / realToFrac (br_runs br2)) / realToFrac (realToFrac (br_alloc br1) / realToFrac (br_runs br1))
    bd_copy   = realToFrac (realToFrac (br_copy br2) / realToFrac (br_runs br2)) / realToFrac (realToFrac (br_copy br1) / realToFrac (br_runs br1))
    bd_coll   = (realToFrac (br_coll br2)) / (realToFrac (br_coll br1))

instance Pretty BenchDiff where
    pretty BenchDiff {..} =
        unlines
            [ ""
            , divider
            , header2
            , divider
            , cpuTimeStats
            , mutTimeStats
            , gcTimeStats
            ]
      where
        header2 = "            Time |    Relative |       Space |    Throughput"

        divider = "     -------------------------------------------------------"

        cpuTimeStats =
          "CPU:"    <> p bd_cpu

        relativeMutationTime :: Multiplier
        relativeMutationTime =
            Multiplier
                (mkPercent (mut bd_bench2) (cpu bd_bench2))
                (mkPercent (mut bd_bench1) (cpu bd_bench1))

        relativeGCTime :: Multiplier
        relativeGCTime =
            Multiplier
                (mkPercent (gc bd_bench2) (cpu bd_bench2))
                (mkPercent (gc bd_bench1) (cpu bd_bench1))

        allocRateMultiplier :: Multiplier
        allocRateMultiplier =
            Multiplier
                (allocRate bd_bench2)
                (allocRate bd_bench1)

        copyRateMultiplier :: Multiplier
        copyRateMultiplier =
            Multiplier
                (copyRate bd_bench2)
                (copyRate bd_bench1)

        mutTimeStats =
          "MUT:"    <> p bd_mut
            <> "  " <> p relativeMutationTime
            <> "  " <> p bd_alloc
            <> "    " <> p allocRateMultiplier

        gcTimeStats =
          "GC: "    <> p bd_gc
            <> "  " <> p relativeGCTime
            <> "  " <> p bd_copy
            <> "    " <> p copyRateMultiplier

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s

report :: BenchResult -> BenchResult -> Benchmark
report br1 br2 = notep $ Report (Pure.Bench.GHC.diff br1 br2)

newtype Report = Report BenchDiff
  deriving (Read,Show,Eq,Generic,ToJSON,FromJSON)

instance Pretty Report where
    pretty (Report (BenchDiff {..})) =
        unlines
            [ ""
            , divider
            , header2
            , divider
            , oldCpuTimeStats
            , newCpuTimeStats
            , cpuTimeDiff
            , ""
            , oldMutTimeStats
            , newMutTimeStats
            , mutTimeDiff
            , ""
            , oldGcTimeStats
            , newGcTimeStats
            , gcTimeDiff
            , ""
            , "Old Collections:" <> pad 11 (pretty (coll bd_bench1))
            , "New Collections:" <> pad 11 (pretty (coll bd_bench2))
            , "Change:         " <> pad 11 (pretty (Multiplier (coll bd_bench1) (coll bd_bench2) :: Multiplier))
            ]
      where
        oldCpuTimeStats =
          "Old CPU:" <> p (cpu bd_bench1)

        newCpuTimeStats =
          "New CPU:" <> p (cpu bd_bench2)

        oldMutTimeStats =
          "Old MUT:"    <> p (mut bd_bench1)
            <> "  " <> p (mkPercent (mut bd_bench1) (cpu bd_bench1))
            <> "  " <> p (alloc bd_bench1)
            <> "  " <> p (allocRate bd_bench1)

        newMutTimeStats =
          "New MUT:"    <> p (mut bd_bench2)
            <> "  " <> p (mkPercent (mut bd_bench2) (cpu bd_bench2))
            <> "  " <> p (alloc bd_bench2)
            <> "  " <> p (allocRate bd_bench2)

        oldGcTimeStats =
          "Old GC: "    <> p (gc bd_bench1)
            <> "  " <> p (mkPercent (gc bd_bench1) (cpu bd_bench1))
            <> "  " <> p (copy bd_bench1)
            <> "  " <> p (copyRate bd_bench1)

        newGcTimeStats =
          "New GC: "    <> p (gc bd_bench2)
            <> "  " <> p (mkPercent (gc bd_bench2) (cpu bd_bench2))
            <> "  " <> p (copy bd_bench2)
            <> "  " <> p (copyRate bd_bench2)

        header2 = "                Time |    Relative |       Space |    Throughput"

        divider = "         -------------------------------------------------------"

        cpuTimeDiff =
          "Change: "    <> p bd_cpu

        relativeMutationTime :: Multiplier
        relativeMutationTime =
          Multiplier (mkPercent (mut bd_bench2) (cpu bd_bench2))
                 (mkPercent (mut bd_bench1) (cpu bd_bench1))

        relativeGCTime :: Multiplier
        relativeGCTime =
          Multiplier (mkPercent (gc bd_bench2) (cpu bd_bench2))
                 (mkPercent (gc bd_bench1) (cpu bd_bench1))

        allocRateMultiplier :: Multiplier
        allocRateMultiplier =
          Multiplier (allocRate bd_bench2) (allocRate bd_bench1)

        copyRateMultiplier :: Multiplier
        copyRateMultiplier =
          Multiplier (copyRate bd_bench2) (copyRate bd_bench1)

        mutTimeDiff =
          "Change: "  <> p bd_mut
            <> "  "   <> p relativeMutationTime
            <> "  "   <> p bd_alloc
            <> "  " <> p allocRateMultiplier

        gcTimeDiff =
          "Change: "  <> p bd_gc
            <> "  "   <> p relativeGCTime
            <> "  "   <> p bd_copy
            <> "  " <> p copyRateMultiplier

        p :: forall p. Pretty p => p -> String
        p = pad 12 . pretty

        pad :: Int -> String -> String
        pad n s =
          let l = length s
          in replicate (n - l) ' ' <> s

summary :: BenchResult -> Benchmark
summary br = do
  s <- currentScope
  notep (Summary s br)

data Summary = Summary String BenchResult

instance Pretty Summary where
  pretty (Summary s br) = 
    s <> pad (max 0 (60 - length s)) (pretty (cpu br))
    where
      pad :: Int -> String -> String
      pad n s =
        let l = length s
        in replicate (n - l) ' ' <> s

{-# INLINE mkBenchResult #-}
mkBenchResult :: Count -> RTSStats -> RTSStats -> BenchResult
mkBenchResult br_runs before after =
  let !rts      = mkRuntimeStats before after
      !br_dur   = rs_cputime rts
      !br_cpu   = rs_cputime rts
      !br_mut   = rs_mutTime rts
      !br_gc    = rs_gcTime  rts
      !br_alloc = rs_allocated rts
      !br_copy  = rs_copied rts
      !br_coll  = rs_collections rts
  in BenchResult {..}

removeOverhead :: BenchResult -> BenchResult -> BenchResult
removeOverhead overhead br =
  BenchResult
    (br_runs br)
    (max 0 (br_dur br - br_dur overhead))
    (max 0 (br_cpu br - br_cpu overhead))
    (max 0 (br_mut br - br_mut overhead))
    (br_gc br)
    (br_alloc br)
    (br_copy br)
    (br_coll br)

{-# INLINE nf #-}
nf :: (NFData a) => (b -> a) -> b -> Test Sync BenchResult
nf f b = io $ do
    (before,after) <- execute 1
    run (mkBenchResult 1 before after)
  where
    {-# INLINE run #-}
    run :: BenchResult -> IO BenchResult
    run br
      | dur br < Milliseconds 100 = do
          let rs = 10 * runs br
          (before,after) <- execute (round rs)
          run $ mkBenchResult rs before after

      | otherwise = do
          let rs = realToFrac $ (Seconds 5 / dur br) * realToFrac (runs br)
          (before,after) <- execute (round rs)
          return $ mkBenchResult rs before after

    {-# INLINE execute #-}
    execute :: Int64 -> IO (RTSStats,RTSStats)
    execute n = do
        performGC
        before <- getRTSStats
        go n f b
        performGC
        after <- getRTSStats
        return (before,after)

      where
        {-# INLINE go #-}
        go 0 f b = return ()
        go n f b = f b `deepseq` go (n - 1) f b

{-# INLINE nfio #-}
nfio :: (NFData a) => IO a -> Test Sync BenchResult
nfio = nf unsafePerformIO 

{-# INLINE whnf #-}
whnf :: (b -> a) -> b -> Test Sync BenchResult
whnf f b = io $ do
    (before,after) <- execute 1
    run (mkBenchResult 1 before after)
  where
    {-# INLINE run #-}
    run :: BenchResult -> IO BenchResult
    run br
      | dur br < Milliseconds 100 = do
          let rs = 10 * runs br
          (before,after) <- execute (round rs)
          run $ mkBenchResult rs before after

      | otherwise = do
          let rs = realToFrac $ (Seconds 5 / dur br) * realToFrac (runs br)
          (before,after) <- execute (round rs)
          return $ mkBenchResult rs before after

    {-# INLINE execute #-}
    execute :: Int64 -> IO (RTSStats,RTSStats)
    execute n = do
        performGC
        before <- getRTSStats
        go n f b
        performGC
        after <- getRTSStats
        return (before,after)

      where
        {-# INLINE go #-}
        go 0 f b = return ()
        go n f b = f b `seq` go (n - 1) f b

{-# INLINE whnfio #-}
whnfio :: IO a -> Test Sync BenchResult
whnfio = whnf unsafePerformIO 

type BenchPred a = a -> BenchResult -> BenchResult -> Bool

data Feature = GC | CPU | MUT | Garbage | Copy | Clock | Allocation | Mutation
  deriving (Eq,Show,Ord,Read,Enum)

data Predicate
  = Feature :>> ()
  | Feature :>  ()
  | Feature :>= ()
  | Feature :=  ()
  | Feature :<= ()
  | Feature :<  ()
  | Feature :<< ()

{-# INLINE constrain #-}
constrain :: BenchResult -> BenchResult -> [Predicate] -> Test sync ()
constrain br1 br2 =
  mapM_ $ \p ->
    let pred =
          case p of
            f :>> () ->
              (" :>>",f,\(Measure s) -> improving (s br1) (s br2) && not (mag (base (s br1)) (s br1) (s br2)))
            f :> () ->
              (" :>",f,\(Measure s) -> improving (s br1) (s  br2) && not (sim (base (s br1)) (s br1) (s br2)))
            f :>= () ->
              (" :>=",f,\(Measure s) -> improving (s br1) (s br2) || sim (base (s br1)) (s br1) (s br2))
            f := () ->
              (" :=",f,\(Measure s) -> sim (base (s br1)) (s br1) (s br2))
            f :<= () ->
              (" :<=",f,\(Measure s) -> improving (s br2) (s br1) || sim (base (s br1)) (s br1) (s br2))
            f :< () ->
              (" :<",f,\(Measure s) -> improving (s br2) (s br1) && not (sim (base (s br1)) (s br1) (s br2)))
            f :<< () ->
              (" :<<",f,\(Measure s) -> improving (s br2) (s br1) && not (mag (base (s br1)) (s br1) (s br2)))
        selector f =
          case f of
            GC  -> Measure gc
            CPU -> Measure cpu
            MUT -> Measure mut

            Garbage -> Measure copy
            Copy    -> Measure copyRate

            Allocation -> Measure alloc
            Mutation   -> Measure allocRate

    in case pred of
        (sc,f,g) -> scope (show f ++ sc) $
          let sel = selector f in
          if g sel then
            ok
          else
            case sel of
              Measure s -> crash $
                intercalate " " [ "Expecting:", pretty (s br1), improvingShow (s br1), pretty (s br2) ]
