module Main (main) where

import Push

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck  as QC
import qualified Data.ByteString.Lazy   as BS

import Data.String.Conversions              (cs)

instance Arbitrary BS.ByteString where
    arbitrary = cs <$> (arbitrary :: Gen String)

prop_treehash :: BS.ByteString -> BS.ByteString -> Bool
prop_treehash x y = if x == y
                      then treeHash x == treeHash y
                      else treeHash x /= treeHash y

treeHashTests :: [TestTree]
treeHashTests = [ testProperty "hash" prop_treehash ]

unitTests :: [TestTree]
unitTests = map mkUnitTest tests

  where

    mkUnitTest (desc, input, output) = testCase desc $ treeHash input @?= output

    tests :: [(String, BS.ByteString, BS.ByteString)]
    tests = map f
        [ ("test001", "",                                             "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        , ("test002", "a",                                            "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
        , ("test003", "b",                                            "3e23e8160039594a33894f6564e1b1348bbd7a0088d42c4acb73eeaed59c009d")
        , ("test004", "c",                                            "2e7d2c03a9507ae265ecf5b5356885a53393a2029d241394997265a1a25aefc6")
        , ("test005", "d",                                            "18ac3e7343f016890c510e93f935261169d9e3f565436429830faf0934f4f8e4")
        , ("test006", "e",                                            "3f79bb7b435b05321651daefd374cdc681dc06faa65e374e38337b88ca046dea")
        , ("test007", "f",                                            "252f10c83610ebca1a059c0bae8255eba2f95be4d1d7bcfa89d7248a82d9f111")
        , ("test007", "The quick brown fox jumps over the lazy dog.", "ef537f25c895bfa782526529a9b63d97aa631564d5d789c2b765448c8635fb6c")
        ]
      where
        f (x, y, z) = (x, cs y, cs z)

main :: IO ()
main = defaultMain $ testGroup "glacier-push"
        [ testGroup "collisionQC"   treeHashTests
        , testGroup "unitTests"     unitTests
        ]
