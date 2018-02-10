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
import qualified Numeric.LinearAlgebra.Static        as H
import qualified Numeric.LinearAlgebra.Static.Vector as H

prop_rVec :: Property
prop_rVec = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10)
                            (Gen.double (Range.linearFracFrom 0 (-10) 10))
    case fromJust $ someNatVal (fromIntegral (length xs)) of
      SomeNat (Proxy :: Proxy n) ->
        tripping xs (H.rVec . H.vector @n)
                    (Identity . UVS.toList . H.extract . H.vecR)

prop_vecR :: Property
prop_vecR = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10)
                            (Gen.double (Range.linearFracFrom 0 (-10) 10))
    VS.withSizedList xs $ \v ->
      tripping v H.vecR (Identity . H.rVec)

genComplex :: Gen H.ℂ
genComplex = (:+) <$> Gen.double (Range.linearFracFrom 0 (-10) 10)
                  <*> Gen.double (Range.linearFracFrom 0 (-10) 10)

prop_cVec :: Property
prop_cVec = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genComplex
    case fromJust $ someNatVal (fromIntegral (length xs)) of
      SomeNat (Proxy :: Proxy n) ->
        tripping xs (H.cVec @n . fromJust . H.create . UVS.fromList)
                    (Identity . UVS.toList . H.extract . H.vecC)

prop_vecC :: Property
prop_vecC = property $ do
    xs <- forAll $ Gen.list (Range.constant 5 10) genComplex
    VS.withSizedList xs $ \v ->
      tripping v H.vecC (Identity . H.cVec)

genMatList :: Gen (SomeNat, SomeNat, [[Double]])
genMatList = do
    m  <- Gen.int (Range.constant 5 10)
    n  <- Gen.int (Range.constant 5 10)
    xs <- (replicateM m . replicateM n) $
      Gen.double (Range.linearFracFrom 0 (-10) 10)
    return ( fromJust . someNatVal . fromIntegral $ m
           , fromJust . someNatVal . fromIntegral $ n
           , xs
           )

prop_lRows :: Property
prop_lRows = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll genMatList
    tripping xs (H.lRows @m @n . H.matrix . concat)
                (Identity . map (UVS.toList . H.extract) . V.toList)

prop_lCols :: Property
prop_lCols = property $ do
    ( SomeNat (Proxy :: Proxy m)
     , SomeNat (Proxy :: Proxy n)
     , xs
     ) <- forAll genMatList
    tripping xs (H.lCols @m @n . H.matrix . concat)
                (Identity . transpose . map (UVS.toList . H.extract) . V.toList)

prop_rowsL :: Property
prop_rowsL = property $ do
    (_, SomeNat (Proxy :: Proxy n), xs)  <- forAll genMatList
    V.withSizedList xs $ \v ->
      tripping v (H.rowsL . V.map (H.vector @n))
                 (Identity . V.map (UVS.toList . H.extract) . H.lRows)

prop_colsL :: Property
prop_colsL = property $ do
    (_, SomeNat (Proxy :: Proxy n), xs)  <- forAll genMatList
    V.withSizedList xs $ \v ->
      tripping v (H.colsL . V.map (H.vector @n))
                 (Identity . V.map (UVS.toList . H.extract) . H.lCols)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- checkParallel $$(discover)

  unless results exitFailure

