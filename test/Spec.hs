import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec             (before, describe, hspec, it, shouldBe)
import           Test.Hspec.Hedgehog    (modifyMaxSuccess, PropertyT, diff, forAll, hedgehog,
                                         (/==), (===))
import Hedgehog (MonadGen, property, Property, withTests, Gen)

import DAFSA.Graph
import Control.Monad (forM_, forM)

main :: IO ()
main = hspec $ do
        describe "fstContains" $ do
            modifyMaxSuccess (const 500) $
                it "FST contains all strings inserted" $ hedgehog $ do
                    words <- forAll (randomStrings 500 (Gen.string (Range.linear 0 20) Gen.unicode))
                    let dafsa = graphFromList words
                    forM_ words (\s -> graphContains s dafsa === True)
            modifyMaxSuccess (const 1) $
                it "empty FST contains nothing" $ hedgehog $ do
                    words <- forAll (randomStrings 500 (Gen.string (Range.linear 0 20) Gen.unicode))
                    let dafsa = emptyGraph
                    forM_ words (\s -> graphContains s dafsa === False)
            modifyMaxSuccess (const 500) $
                it "FST should not contain prefixes of words" $ hedgehog $ do
                    -- each string generated will have at leas length 10, so DAFSA should not contain any ot their prefixes
                    words <- forAll (randomStrings 500 (Gen.string (Range.linear 10 20) Gen.unicode))
                    let prefixes = take 9 <$> words
                    let dafsa = graphFromList words
                    forM_ prefixes (\s -> graphContains s dafsa === False)

                    

randomStrings :: MonadGen m => Int -> m String -> m [String]
randomStrings inTotal = Gen.list (Range.linear 0 inTotal)

