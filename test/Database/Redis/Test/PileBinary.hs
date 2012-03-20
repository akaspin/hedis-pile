-- | Binary tests.

module Database.Redis.Test.PileBinary (tests) where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Data.Binary

tests :: Test
tests = mutuallyExclusive $ testGroup "Pile" [
--    testCase "New Data" caseNewData,
--    testCase "Stored data" caseStoredData
    ]