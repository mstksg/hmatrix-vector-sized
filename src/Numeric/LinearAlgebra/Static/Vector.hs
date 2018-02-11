{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Numeric.LinearAgelbra.Static.Vector
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Conversions between statically sized types in
-- "Numeric.LinearAlgebra.Static" from /hmatrix/ and /vector-sized/.
--
-- This module is intentionally minimal, exporting only functions that
-- cannot be written without "unsafe" operations.  With these, however, you
-- can easy write other useful combinators by using type-safe operations
-- like 'fmap', 'VS.map', 'liftA2', etc.
--

module Numeric.LinearAlgebra.Static.Vector (
  -- * Vector
  -- ** Real
    rVec
  , vecR
  -- ** Complex
  , cVec
  , vecC
  -- * Matrix
  -- ** Real
  , lRows
  , rowsL
  , lCols
  , colsL
  -- ** Complex
  , mRows
  , rowsM
  , mCols
  , colsM
  ) where

import           Data.Foldable
import           Unsafe.Coerce
import qualified Data.Vector                  as UV
import qualified Data.Vector.Sized            as V
import qualified Data.Vector.Storable.Sized   as VS
import qualified Numeric.LinearAlgebra        as HU
import qualified Numeric.LinearAlgebra.Static as H

-- | Convert an /hmatrix/ vector (parameterized by its lenth) to
-- a /vector-sized/ storable vector of 'Double's.
rVec :: H.R n -> VS.Vector n H.ℝ
rVec = unsafeCoerce

-- | Convert a /vector-sized/ storable vector to an /hmatrix/ vector
-- (parameterized by its lenth).
vecR :: VS.Vector n H.ℝ -> H.R n
vecR = unsafeCoerce

-- | Convert an /hmatrix/ complex vector (parameterized by its lenth) to
-- a /vector-sized/ storable vector of 'Complex Double's, preserving the
-- length in the type.
cVec :: H.C n -> VS.Vector n H.ℂ
cVec = unsafeCoerce

-- | Convert a /vector-sized/ storable vector to an /hmatrix/ complex
-- vector (parameterized by its lenth), preserving the length in the type.
vecC :: VS.Vector n H.ℂ -> H.C n
vecC = unsafeCoerce

-- | Split an /hmatrix/ matrix (parameterized by its dimensions) to
-- a /vector-sized/ boxed vector of its rows (as /hmatrix/ vectors).
lRows
    :: forall m n. ()
    => H.L m n
    -> V.Vector m (H.R n)
lRows = unsafeCoerce
      . UV.fromList
      . HU.toRows
      . (unsafeCoerce :: H.L m n -> HU.Matrix Double)

-- | Join together a /vector-sized/ boxed vector of /hmatrix/ vectors to an
-- /hmatrix/ matrix as its rows.
rowsL
    :: forall m n. ()
    => V.Vector m (H.R n)
    -> H.L m n
rowsL = (unsafeCoerce :: HU.Matrix Double -> H.L m n)
      . HU.fromRows
      . map (unsafeCoerce :: H.R n -> HU.Vector Double)
      . toList

-- | Split an /hmatrix/ matrix (parameterized by its dimensions) to
-- a /vector-sized/ boxed vector of its columns (as /hmatrix/ vectors).
lCols
    :: forall m n. ()
    => H.L m n
    -> V.Vector n (H.R m)
lCols = unsafeCoerce
      . UV.fromList
      . HU.toColumns
      . (unsafeCoerce :: H.L m n -> HU.Matrix Double)

-- | Join together a /vector-sized/ boxed vector of /hmatrix/ vectors to an
-- /hmatrix/ matrix as its columns.
colsL
    :: forall m n. ()
    => V.Vector n (H.R m)
    -> H.L m n
colsL = (unsafeCoerce :: HU.Matrix Double -> H.L m n)
      . HU.fromColumns
      . map (unsafeCoerce :: H.R m -> HU.Vector Double)
      . toList

-- | Split an /hmatrix/ complex matrix (parameterized by its dimensions) to
-- a /vector-sized/ boxed vector of its rows (as /hmatrix/ complex
-- vectors).
mRows
    :: forall m n. ()
    => H.M m n
    -> V.Vector m (H.C n)
mRows = unsafeCoerce
      . UV.fromList
      . HU.toRows
      . (unsafeCoerce :: H.M m n -> HU.Matrix H.ℂ)

-- | Join together a /vector-sized/ boxed vector of /hmatrix/ complex
-- vectors to an /hmatrix/ complex matrix as its rows.
rowsM
    :: forall m n. ()
    => V.Vector m (H.C n)
    -> H.M m n
rowsM = (unsafeCoerce :: HU.Matrix H.ℂ -> H.M m n)
      . HU.fromRows
      . map (unsafeCoerce :: H.C n -> HU.Vector H.ℂ)
      . toList

-- | Split an /hmatrix/ complex matrix (parameterized by its dimensions) to
-- a /vector-sized/ boxed vector of its columns (as /hmatrix/ complex
-- vectors).
mCols
    :: forall m n. ()
    => H.M m n
    -> V.Vector n (H.C m)
mCols = unsafeCoerce
      . UV.fromList
      . HU.toColumns
      . (unsafeCoerce :: H.M m n -> HU.Matrix H.ℂ)

-- | Join together a /vector-sized/ boxed vector of /hmatrix/ complex
-- vectors to an /hmatrix/ complex matrix as its columns.
colsM
    :: forall m n. ()
    => V.Vector n (H.C m)
    -> H.M m n
colsM = (unsafeCoerce :: HU.Matrix H.ℂ -> H.M m n)
      . HU.fromColumns
      . map (unsafeCoerce :: H.C m -> HU.Vector H.ℂ)
      . toList
