module Numeric.LinearAlgebra.Static.Vector (
    rVec
  , vecR
  , cVec
  , vecC
  , lRows
  , rowsL
  , lCols
  , colsL
  ) where

import           Data.Foldable
import           Data.Maybe
import           GHC.TypeLits
import           Text.Printf
import qualified Data.Vector.Sized            as V
import qualified Data.Vector.Storable         as UVS
import qualified Data.Vector.Storable.Sized   as VS
import qualified Numeric.LinearAlgebra.Static as H

rVec :: KnownNat n => H.R n -> VS.Vector n H.ℝ
rVec x = fromMaybe (error e) $ VS.toSized y
  where
    y = H.extract x
    e = printf "rVec: Unexpected size mismatch (expected %d, got %d)"
          (H.size x) (UVS.length y)

vecR :: KnownNat n => VS.Vector n H.ℝ -> H.R n
vecR x = fromMaybe (error e) $ H.create y
  where
    y = VS.fromSized x
    e = printf "vecR: Unexpected size mismatch (expected %d, got %d)"
          (VS.length x) (UVS.length y)

cVec :: KnownNat n => H.C n -> VS.Vector n H.ℂ
cVec x = fromMaybe (error e) $ VS.toSized y
  where
    y = H.extract x
    e = printf "rVec: Unexpected size mismatch (expected %d, got %d)"
          (H.size x) (UVS.length y)

vecC :: KnownNat n => VS.Vector n H.ℂ -> H.C n
vecC x = fromMaybe (error e) $ H.create y
  where
    y = VS.fromSized x
    e = printf "vecR: Unexpected size mismatch (expected %d, got %d)"
          (VS.length x) (UVS.length y)

lRows
    :: (KnownNat m, KnownNat n)
    => H.L m n
    -> V.Vector m (H.R n)
lRows x = fromMaybe (error e) $ V.fromList y
  where
    y = H.toRows x
    e = printf "lRows: Unexpected length mismatch (expected %d, got %d)"
          (fst (H.size x)) (length y)

rowsL
    :: (KnownNat m, KnownNat n)
    => V.Vector m (H.R n)
    -> H.L m n
rowsL x = H.withRows y $ \z ->
            fromMaybe (error e) (H.exactDims z)
  where
    y = toList x
    e = printf "rowsL: Unexpected length mismatch (expected %d, got %d)"
          (V.length x) (length y)

lCols
    :: (KnownNat m, KnownNat n)
    => H.L m n
    -> V.Vector n (H.R m)
lCols x = fromMaybe (error e) $ V.fromList y
  where
    y = H.toColumns x
    e = printf "lCols: Unexpected length mismatch (expected %d, got %d)"
          (snd (H.size x)) (length y)

colsL
    :: (KnownNat m, KnownNat n)
    => V.Vector n (H.R m)
    -> H.L m n
colsL x = H.withColumns y $ \z ->
            fromMaybe (error e) (H.exactDims z)
  where
    y = toList x
    e = printf "colsL: Unexpected length mismatch (expected %d, got %d)"
          (V.length x) (length y)

