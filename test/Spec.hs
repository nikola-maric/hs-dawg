import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec             (before, describe, hspec, it, shouldBe)
import           Test.Hspec.Hedgehog    (modifyMaxSuccess, PropertyT, diff, forAll, hedgehog,
                                         (/==), (===))
import Hedgehog (MonadGen, property, Property, withTests, Gen)

import DAFSA.Graph
import Control.Monad (forM_, forM)
import Control.Monad.ST (runST)
import qualified Data.IntSet as IS

main :: IO ()
main = hspec $ do
        describe "fstContains" $ do
            modifyMaxSuccess (const 2000) $
                it "FST contains all strings inserted, small strings" $ hedgehog $ do
                    words <- forAll (randomStrings 200 (Gen.string (Range.linear 0 10) Gen.unicode))
                    let dafsa = fromWords words
                    forM_ words (\s -> contains s dafsa === True)
            modifyMaxSuccess (const 1000) $
                it "FST contains all strings inserted, large strings" $ hedgehog $ do
                        words <- forAll (randomStrings 200 (Gen.string (Range.linear 0 50) Gen.unicode))
                        let dafsa = fromWords words
                        forM_ words (\s -> contains s dafsa === True)
            modifyMaxSuccess (const 1) $
                it "empty FST contains nothing" $ hedgehog $ do
                    words <- forAll (randomStrings 100 (Gen.string (Range.linear 0 5) Gen.unicode))
                    let dafsa = fromWords []
                    forM_ words (\s -> contains s dafsa === False)
                    contains "" dafsa === False
            modifyMaxSuccess (const 1) $
                it "FST can contain only empty string" $ hedgehog $ do
                    let dafsa = fromWords [""]
                    contains "" dafsa === True
            modifyMaxSuccess (const 1000) $
                it "FST should not contain prefixes of words" $ hedgehog $ do
                    -- each string generated will have at least length 10, so DAFSA should not contain any ot their prefixes
                    words <- forAll (randomStrings 200 (Gen.string (Range.linear 10 20) Gen.unicode))
                    let prefixes = take 9 <$> words
                    let dafsa = fromWords words
                    forM_ prefixes (\s -> contains s dafsa === False)
        -- describe "DAFSA structure" $ do
        --         it "checking for predetrmined FST structure" $ example $ do
        --             let words = ["mon", "thurs", "tues", "zon"]
        --                 dafsa = fromWords words
        --             pure (IS.toList (fgFinalStates dafsa)) === [3]

                    

randomStrings :: MonadGen m => Int -> m String -> m [String]
randomStrings inTotal = Gen.list (Range.linear 0 inTotal)

