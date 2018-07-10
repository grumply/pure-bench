{-# LANGUAGE CPP #-}
module Pure.Bench (module Export) where

#ifdef __GHCJS__
import Pure.Bench.GHCJS as Export
#else
import Pure.Bench.GHC as Export
#endif
