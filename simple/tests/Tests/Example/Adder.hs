module Tests.Example.Adder where

import Prelude

import Clash.Hedgehog.Sized.Unsigned
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Import module containing the @adder@ function
import Example.Adder (adder)

-- Define a Hedgehog property to test the @adder@ function
prop_adder :: H.Property
prop_adder = H.property $ do

    -- Simulate for a random duration between 1 and 100 cycles
    -- simDuration <- H.forAll (Gen.integral (Range.linear 1 100))
    simDuration <- H.forAll (Gen.integral (Range.linear 5 5))

    -- Generate a list of random unsigned numbers
    inp_a <- H.forAll
        (Gen.list (Range.singleton simDuration)
        (genUnsigned Range.linearBounded))
    inp_b <- H.forAll
        (Gen.list (Range.singleton simDuration)
        (genUnsigned Range.linearBounded))
    let

        -- Simulate the @addr@ function for the pre-existing @System@ domain
        -- and 8 bit unsigned numbers.
        simOut = C.sampleN (simDuration + 1) (adder @C.System @8
                                            C.clockGen
                                            C.resetGen
                                            C.enableGen
                                            (C.fromList inp_a)
                                            (C.fromList inp_b))
        expected = 0 : zipWith (+) inp_a inp_b

    -- Check that the simulated output matches the expected output
    simOut H.=== expected

adderTests :: TestTree
adderTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain adderTests

