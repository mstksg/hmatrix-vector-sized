{-# LANGUAGE CPP                                      #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeOperators                            #-}
{-# OPTIONS_GHC -fno-warn-orphans                     #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType         #-}
#endif

import           Control.Monad
import           Data.Complex
import           Data.Function
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import           Hedgehog
import           Numeric.Natural
import           System.Exit
import           System.IO
import qualified Data.Vector.Sized                   as V
import qualified Data.Vector.Storable.Sized          as VS
import qualified Hedgehog.Gen                        as Gen
import qualified Hedgehog.Range                      as Range
import qualified Numeric.LinearAlgebra               as HU
import qualified Numeric.LinearAlgebra.Static        as H
import qualified Numeric.LinearAlgebra.Static.Vector as H

instance KnownNat n => Eq (H.R n) where
    (==) = (==) `on` H.extract

instance KnownNat n => Eq (H.C n) where
    (==) = (==) `on` H.extract

instance (KnownNat m, KnownNat n) => Eq (H.L m n) where
    (==) = (==) `on` H.extract

instance (KnownNat m, KnownNat n) => Eq (H.M m n) where
    (==) = (==) `on` H.extract

someNatVal' :: Natural -> SomeNat
someNatVal' = fromJust . someNatVal . fromIntegral

genDouble :: Gen H.ℝ
genDouble = Gen.double (Range.linearFracFrom 0 (-10) 10)

genComplex :: Gen H.ℂ
genComplex = (:+) <$> genDouble <*> genDouble

sizeRange :: Num a => Range a
sizeRange = Range.constant 5 10

prop_rVec :: Property
prop_rVec = property $ do
    xs <- forAll $ Gen.list sizeRange genDouble
    SomeNat (Proxy :: Proxy n) <- pure $ someNatVal' (fromIntegral (length xs))
    tripping (H.vector @n xs)
             (VS.map (* 2) . H.rVec)
             (Identity . (/ 2) . H.vecR)

prop_rVec_konst :: Property
prop_rVec_konst = property $ do
    x <- forAll genDouble
    SomeNat (Proxy :: Proxy n) <- fmap someNatVal' . forAll
                                $ Gen.integral sizeRange
    tripping (H.konst @_ @(H.R n) x)
             (VS.map (* 2) . H.rVec)
             (Identity . (/ 2) . H.vecR)

prop_vecR :: Property
prop_vecR = property $ do
    xs <- forAll $ Gen.list sizeRange genDouble
    VS.withSizedList xs $ \v ->
      tripping v ((* 2) . H.vecR)
                 (Identity . VS.map (/ 2) . H.rVec)

prop_cVec :: Property
prop_cVec = property $ do
    xs <- forAll $ Gen.list sizeRange genComplex
    SomeNat (Proxy :: Proxy n) <- pure $ someNatVal' (fromIntegral (length xs))
    tripping (fromJust . H.create $ HU.fromList xs)
             (VS.map (* 2) . H.cVec @n)
             (Identity . (/ 2) . H.vecC)

prop_cVec_konst :: Property
prop_cVec_konst = property $ do
    x <- forAll genComplex
    SomeNat (Proxy :: Proxy n) <- fmap someNatVal' . forAll
                                $ Gen.integral sizeRange
    tripping (H.konst @_ @(H.C n) x)
             (VS.map (* 2) . H.cVec)
             (Identity . (/ 2) . H.vecC)

prop_vecC :: Property
prop_vecC = property $ do
    xs <- forAll $ Gen.list sizeRange genComplex
    VS.withSizedList xs $ \v ->
      tripping v ((* 2) . H.vecC)
                 (Identity . VS.map (/ 2) . H.cVec)

genMatList :: Gen a -> Gen (SomeNat, SomeNat, [[a]])
genMatList g = do
    m  <- Gen.integral sizeRange
    n  <- Gen.integral sizeRange
    xs <- (replicateM (fromIntegral m) . replicateM (fromIntegral n)) g
    return ( someNatVal' m
           , someNatVal' n
           , xs
           )

prop_lRows :: Property
prop_lRows = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genDouble
    tripping (fromJust . H.create $ HU.fromLists xs)
             (V.map (* 2) . H.lRows @m @n)
             (Identity . (/ 2) . H.rowsL)

prop_rowsL :: Property
prop_rowsL = property $ do
    (_, SomeNat (Proxy :: Proxy n), xs)  <- forAll $ genMatList genDouble
    V.withSizedList xs $ \(v :: V.Vector m [Double]) ->
      tripping (V.map (fromJust . H.create . HU.fromList) v)
               ((* 2) . H.rowsL @m @n)
               (Identity . V.map (/ 2) . H.lRows)

prop_lCols :: Property
prop_lCols = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genDouble
    tripping (fromJust . H.create $ HU.fromLists xs)
             (V.map (* 2) . H.lCols @m @n)
             (Identity . (/ 2) . H.colsL)

prop_colsL :: Property
prop_colsL = property $ do
    (_, SomeNat (Proxy :: Proxy m), xs)  <- forAll $ genMatList genDouble
    V.withSizedList xs $ \(v :: V.Vector n [Double]) ->
      tripping (V.map (fromJust . H.create . HU.fromList) v)
               ((* 2) . H.colsL @m @n)
               (Identity . V.map (/ 2) . H.lCols)

prop_vecL :: Property
prop_vecL = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genDouble
    let v :: VS.Vector (m * n) Double
        v = fromJust $ VS.fromList @_ @(m * n) (concat xs)
    tripping v ((* 2) . H.vecL @m @n)
              (Identity . VS.map (/ 2) . H.lVec)

prop_lVec :: Property
prop_lVec = property $ do
    ( SomeNat (pM@Proxy :: Proxy m)
     , SomeNat (pN@Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genDouble
    let m :: H.L m n
        m = fromJust
          . H.create
          . (fromIntegral (natVal pM) HU.>< fromIntegral (natVal pN))
          $ concat xs
    tripping m (VS.map (* 2) . H.lVec)
              (Identity . (/ 2) . H.vecL)

prop_lVec_konst :: Property
prop_lVec_konst = property $ do
    x <- forAll genDouble
    SomeNat (Proxy :: Proxy m) <- fmap someNatVal' . forAll
                                $ Gen.integral sizeRange
    SomeNat (Proxy :: Proxy n) <- fmap someNatVal' . forAll
                                $ Gen.integral sizeRange
    tripping (H.konst @_ @(H.L m n) x)
             (VS.map (* 2) . H.lVec)
             (Identity . (/ 2) . H.vecL)

prop_mRows :: Property
prop_mRows = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genComplex
    tripping (fromJust . H.create $ HU.fromLists xs)
             (V.map (* 2) . H.mRows @m @n)
             (Identity . (/ 2) . H.rowsM)

prop_rowsM :: Property
prop_rowsM = property $ do
    (_, SomeNat (Proxy :: Proxy n), xs)  <- forAll $ genMatList genComplex
    V.withSizedList xs $ \(v :: V.Vector m [Complex Double]) ->
      tripping (V.map (fromJust . H.create . HU.fromList) v)
               ((* 2) . H.rowsM @m @n)
               (Identity . V.map (/ 2) . H.mRows)

prop_mCols :: Property
prop_mCols = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genComplex
    tripping (fromJust . H.create $ HU.fromLists xs)
             (V.map (* 2) . H.mCols @m @n)
             (Identity . (/ 2) . H.colsM)

prop_colsM :: Property
prop_colsM = property $ do
    (_, SomeNat (Proxy :: Proxy m), xs)  <- forAll $ genMatList genComplex
    V.withSizedList xs $ \(v :: V.Vector n [Complex Double]) ->
      tripping (V.map (fromJust . H.create . HU.fromList) v)
               ((* 2) . H.colsM @m @n)
               (Identity . V.map (/ 2) . H.mCols)

prop_vecM :: Property
prop_vecM = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genComplex
    let v :: VS.Vector (m * n) (Complex Double)
        v = fromJust $ VS.fromList @_ @(m * n) (concat xs)
    tripping v ((* 2) . H.vecM @m @n)
              (Identity . VS.map (/ 2) . H.mVec)

prop_mVec :: Property
prop_mVec = property $ do
    ( SomeNat (pM@Proxy :: Proxy m)
     , SomeNat (pN@Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genComplex
    let m :: H.M m n
        m = fromJust
          . H.create
          . (fromIntegral (natVal pM) HU.>< fromIntegral (natVal pN))
          $ concat xs
    tripping m (VS.map (* 2) . H.mVec)
               (Identity . (/ 2) . H.vecM)

prop_mVec_konst :: Property
prop_mVec_konst = property $ do
    x <- forAll genComplex
    SomeNat (Proxy :: Proxy m) <- fmap someNatVal' . forAll
                                $ Gen.integral sizeRange
    SomeNat (Proxy :: Proxy n) <- fmap someNatVal' . forAll
                                $ Gen.integral sizeRange
    tripping (H.konst @_ @(H.M m n) x)
             (VS.map (* 2) . H.mVec)
             (Identity . (/ 2) . H.vecM)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- checkParallel $$(discover)

  unless results exitFailure

