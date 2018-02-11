{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Monad
import           Data.Complex
import           Data.Function
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import           Hedgehog
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

genDouble :: Gen H.ℝ
genDouble = Gen.double (Range.linearFracFrom 0 (-10) 10)

genComplex :: Gen H.ℂ
genComplex = (:+) <$> genDouble <*> genDouble

prop_rVec :: Property
prop_rVec = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genDouble
    case fromJust $ someNatVal (fromIntegral (length xs)) of
      SomeNat (Proxy :: Proxy n) ->
        tripping (H.vector @n xs)
                 (VS.map (* 2) . H.rVec)
                 (Identity . (/ 2) . H.vecR)

prop_vecR :: Property
prop_vecR = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genDouble
    VS.withSizedList xs $ \v ->
      tripping v ((* 2) . H.vecR)
                 (Identity . VS.map (/ 2) . H.rVec)

prop_cVec :: Property
prop_cVec = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genComplex
    case fromJust $ someNatVal (fromIntegral (length xs)) of
      SomeNat (Proxy :: Proxy n) ->
        tripping (fromJust . H.create $ HU.fromList xs)
                 (VS.map (* 2) . H.cVec @n)
                 (Identity . (/ 2) . H.vecC)

prop_vecC :: Property
prop_vecC = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genComplex
    VS.withSizedList xs $ \v ->
      tripping v ((* 2) . H.vecC)
                 (Identity . VS.map (/ 2) . H.cVec)

genMatList :: Gen a -> Gen (SomeNat, SomeNat, [[a]])
genMatList g = do
    m  <- Gen.int (Range.constant 5 10)
    n  <- Gen.int (Range.constant 5 10)
    xs <- (replicateM m . replicateM n) g
    return ( fromJust . someNatVal . fromIntegral $ m
           , fromJust . someNatVal . fromIntegral $ n
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- checkParallel $$(discover)

  unless results exitFailure

