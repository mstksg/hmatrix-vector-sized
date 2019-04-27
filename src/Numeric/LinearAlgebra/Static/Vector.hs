{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType         #-}
#endif

-- |
-- Module      : Numeric.LinearAlgebra.Static.Vector
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
-- can easily write other useful combinators by using type-safe operations
-- like 'fmap', 'VS.map', 'liftA2', 'Data.Vector.Generic.Sized.convert',
-- etc.
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
  , lVec
  , vecL
  -- ** Complex
  , mRows
  , rowsM
  , mCols
  , colsM
  , mVec
  , vecM
  ) where

import           Data.Foldable
import           Data.Proxy
import           GHC.TypeLits
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
      . (unsafeCoerce :: [H.R n] -> [HU.Vector Double])
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
      . (unsafeCoerce :: [H.R m] -> [HU.Vector Double])
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
      . (unsafeCoerce :: [H.C n] -> [HU.Vector H.ℂ])
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
      . (unsafeCoerce :: [H.C m] -> [HU.Vector H.ℂ])
      . toList

-- | Shape a /vector-sized/ storable vector of elements into an /hmatrix/
-- matrix.
--
-- @since 0.1.1.0
vecL
    :: forall m n. KnownNat n
    => VS.Vector (m * n) H.ℝ
    -> H.L m n
vecL = (unsafeCoerce :: HU.Matrix H.ℝ -> H.L m n)
     . HU.reshape (fromIntegral (natVal (Proxy @n)))
     . unsafeCoerce

-- | Flatten an /hmatrix/ matrix into a /vector-sized/ storable vector of
-- its items.
--
-- @since 0.1.1.0
lVec
    :: forall m n. ()
    => H.L m n
    -> VS.Vector (m * n) H.ℝ
lVec = unsafeCoerce
     . HU.flatten
     . (unsafeCoerce :: H.L m n -> HU.Matrix H.ℝ)

-- | Shape a /vector-sized/ storable vector of elements into an /hmatrix/
-- complex matrix.
--
-- @since 0.1.1.0
vecM
    :: forall m n. KnownNat n
    => VS.Vector (m * n) H.ℂ
    -> H.M m n
vecM = (unsafeCoerce :: HU.Matrix H.ℂ -> H.M m n)
     . HU.reshape (fromIntegral (natVal (Proxy @n)))
     . unsafeCoerce

-- | Flatten an /hmatrix/ complex matrix into a /vector-sized/ storable
-- vector of its items.
--
-- @since 0.1.1.0
mVec
    :: forall m n. ()
    => H.M m n
    -> VS.Vector (m * n) H.ℂ
mVec = unsafeCoerce
     . HU.flatten
     . (unsafeCoerce :: H.M m n -> HU.Matrix H.ℂ)

