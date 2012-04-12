{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Database.Redis.Test.Pile (tests) where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Lifted (bracket_)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.String.Conversions ((<>), cs)

import qualified Database.Redis as R
import qualified Database.Redis.Pile as RP

tests :: Test
tests = mutuallyExclusive $ testGroup "Pile" [
    testCase "Just Put & Get" casePutGet
--    testCase "Stored data" caseStoredData
    ]



runInRedis :: forall b. R.Redis b -> IO b
runInRedis a = do
    conn <- R.connect R.defaultConnectInfo
    R.runRedis conn a

casePutGet :: Assertion
casePutGet = bracket_ 
    setup
    teardown $ runInRedis $ do
        r <- RP.pile testPrefix (toBs 1) Nothing $ 
                return (testData 1, "exp", [], Nothing)
        liftIO $ r @=? Just (testData 1)
    
setup :: IO ()
setup = runInRedis $ 
    void $ R.hmset (testPrefix <> ":mark") [("mark", "mark")]

-- | Purge all keys with 'allPrefix'
teardown :: IO ()
teardown = runInRedis $ do
    a <- R.keys $ testPrefix <> "*"
    _ <- either undefined R.del a
    return ()
    
testPrefix :: B.ByteString
testPrefix = "piletest"

testData :: Int -> (Int, [(B.ByteString, Maybe B.ByteString)], BL.ByteString)
testData n = 
    (n, [(toBs n, Just $ toBs n)], toLBs n)

toBs :: Int -> B.ByteString
toBs n = cs . show $ n

toLBs :: Int -> BL.ByteString
toLBs n = cs . show $ n
