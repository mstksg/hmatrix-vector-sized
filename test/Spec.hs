{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

import           Control.Monad
import           Data.Complex
import           Data.Functor.Identity
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           GHC.TypeLits
import           Hedgehog
import           System.Exit
import           System.IO
import qualified Data.Vector.Sized                   as V
import qualified Data.Vector.Storable                as UVS
import qualified Data.Vector.Storable.Sized          as VS
import qualified Hedgehog.Gen                        as Gen
import qualified Hedgehog.Range                      as Range
import qualified Numeric.LinearAlgebra               as HU
import qualified Numeric.LinearAlgebra.Static        as H
import qualified Numeric.LinearAlgebra.Static.Vector as H

genDouble :: Gen H.ℝ
genDouble = Gen.double (Range.linearFracFrom 0 (-10) 10)

genComplex :: Gen H.ℂ
genComplex = (:+) <$> genDouble <*> genDouble

prop_rVec :: Property
prop_rVec = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genDouble
    case fromJust $ someNatVal (fromIntegral (length xs)) of
      SomeNat (Proxy :: Proxy n) ->
        tripping xs ((* 2) . H.rVec . H.vector @n)
                    (Identity . UVS.toList . H.extract . (/ 2) . H.vecR)

prop_vecR :: Property
prop_vecR = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genDouble
    VS.withSizedList xs $ \v ->
      tripping v ((* 2) . H.vecR) (Identity . (/ 2) . H.rVec)

prop_cVec :: Property
prop_cVec = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genComplex
    case fromJust $ someNatVal (fromIntegral (length xs)) of
      SomeNat (Proxy :: Proxy n) ->
        tripping xs ((* 2) . H.cVec @n . fromJust . H.create . UVS.fromList)
                    (Identity . UVS.toList . H.extract . (/ 2) . H.vecC)

prop_vecC :: Property
prop_vecC = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genComplex
    VS.withSizedList xs $ \v ->
      tripping v ((* 2) . H.vecC) (Identity . (/ 2) . H.cVec)

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
    tripping xs ((* 2) . H.lRows @m @n . H.matrix . concat)
                (Identity . map (UVS.toList . H.extract) . H.toRows . (/ 2) . H.rowsL)

prop_rowsL :: Property
prop_rowsL = property $ do
    (_, SomeNat (Proxy :: Proxy n), xs)  <- forAll $ genMatList genDouble
    V.withSizedList xs $ \v ->
      tripping v ((* 2) . H.rowsL . V.map (H.vector @n))
                 (Identity . V.map (UVS.toList . H.extract . (/ 2)) . H.lRows)

prop_lCols :: Property
prop_lCols = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genDouble
    tripping xs ((* 2) . H.lCols @m @n . H.matrix . concat)
                (Identity . transpose . map (UVS.toList . H.extract) . H.toColumns . (/ 2) . H.colsL)

prop_colsL :: Property
prop_colsL = property $ do
    (_, SomeNat (Proxy :: Proxy n), xs)  <- forAll $ genMatList genDouble
    V.withSizedList xs $ \v ->
      tripping v ((* 2) . H.colsL . V.map (H.vector @n))
                 (Identity . V.map (UVS.toList . H.extract . (/ 2)) . H.lCols)

prop_mRows :: Property
prop_mRows = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genComplex
    tripping xs ((* 2) . H.mRows @m @n . fromJust . H.create . HU.fromLists)
                (Identity . map UVS.toList . HU.toRows . H.extract . (/ 2) . H.rowsM)

prop_rowsM :: Property
prop_rowsM = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     )  <- forAll $ genMatList genComplex
    V.withSizedList xs $ \v ->
      tripping v ((* 2) . H.rowsM . V.map (fromJust @(H.C n) . H.create . HU.fromList))
                 (Identity . V.map (UVS.toList @H.ℂ . H.extract . (/ 2)) . H.mRows)

prop_mCols :: Property
prop_mCols = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll $ genMatList genComplex
    tripping xs ((* 2) . H.mCols @m @n . fromJust . H.create . HU.fromLists)
                (Identity . map UVS.toList . HU.toRows . H.extract . (/ 2) . H.colsM)

prop_colsM :: Property
prop_colsM = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     )  <- forAll $ genMatList genComplex
    V.withSizedList xs $ \v ->
      tripping v ((* 2) . H.colsM . V.map (fromJust @(H.C n) . H.create . HU.fromList))
                 (Identity . V.map (UVS.toList @H.ℂ . H.extract . (/ 2)) . H.mCols)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- checkParallel $$(discover)

  unless results exitFailure

