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

import qualified Database.Redis as R
import qualified Database.Redis.Pile as RP

tests :: Test
tests = mutuallyExclusive $ testGroup "Pile" [
    testCase "New Data" caseNewData,
    testCase "Stored data" caseStoredData
    ]

caseNewData :: Assertion
caseNewData = bracket_
    setup
    teardown
    $ runInRedis $ do
        r <- RP.pile allPrefix "two" Nothing $ 
            return (allData, Nothing, Nothing)
        liftIO $ Just allData @=? r
        void $ RP.pile allPrefix "three" Nothing $ 
            return (allData, Just 15, Just ["one"])
        ex <- R.ttl $ allPrefix `B.append` ":three"
        liftIO $ Right 15 @=? ex

caseStoredData :: Assertion
caseStoredData = bracket_
    setup
    teardown
    $ runInRedis $ do
        r <- RP.pile allPrefix "one" Nothing $ 
            return (allData, Nothing, Nothing)
        liftIO $ Just allData @=? r
        r' <- RP.pile allPrefix "one" (Just ("etag", "etag")) $ 
            return (allData, Nothing, Nothing)
        liftIO $ Nothing @=? r'

runInRedis :: forall b. R.Redis b -> IO b
runInRedis a = do
    conn <- R.connect R.defaultConnectInfo
    R.runRedis conn a
    
setup :: IO ()
setup = runInRedis $ 
    void $ R.hmset "piletest:one" allData

-- | Purge all keys with 'allPrefix'
teardown :: IO ()
teardown = runInRedis $ do
    a <- R.keys $ allPrefix `B.append` "*"
    _ <- either undefined R.del a
    return ()
    
allPrefix :: B.ByteString
allPrefix = "piletest"

allData :: [(B.ByteString, B.ByteString)]
allData = [("etag", "etag"), ("anydata", "anydata")]