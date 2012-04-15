{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

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

import Data.Binary (encode, decode)

tests :: Test
tests = mutuallyExclusive $ testGroup "Pile" [
    testCase "Binary test" caseBinary,
    testCase "Just Put & Get" casePutGet,
    testCase "Put and get without expect" caseWithoutTag,
    testCase "Put and get with expect" caseWithTag
    ]

-- | Binary checks
caseBinary :: Assertion
caseBinary = do
    let d1 = (1 :: Int, "a" :: B.ByteString)
    let a1 = encode d1
    let d2 = decode a1 :: (Int, B.ByteString)
    liftIO $ d1 @=? d2

-- | Just do put-get routine
casePutGet :: Assertion
casePutGet = bracket_ 
    setup
    teardown $ runInRedis $ do
        r <- RP.pile testPrefix (toBs 1) Nothing $ 
                return (testData 1, "exp", [], Nothing)
        liftIO $ r @=? Just (testData 1)

-- | Work without tag
caseWithoutTag :: Assertion
caseWithoutTag = bracket_
    setup
    teardown $ runInRedis $ do
        r1 <- RP.pile testPrefix (toBs 1) Nothing $ 
                return (testData 1, "exp", [], Nothing)
        (r2 :: Maybe TData) <- RP.pile testPrefix (toBs 1) Nothing $ 
                return (testData 1, "exp", [], Nothing)
        liftIO $ r1 @=? r2

-- | Work with tag
caseWithTag :: Assertion
caseWithTag = bracket_
    setup
    teardown $ runInRedis $ do
        -- prepend data
        _ <- RP.pile testPrefix (toBs 1) (Just "exp") $ 
                return (testData 1, "exp", [], Nothing)
        -- retrieve with matching expect
        r2 <- RP.pile testPrefix (toBs 1) (Just "exp") $ 
                return (testData 1, "exp", [], Nothing)
        liftIO $ r2 @=? Nothing
        -- retrieve with unmatching expect
        r3 <- RP.pile testPrefix (toBs 1) (Just "exp_no_match") $ 
                return (testData 1, "exp", [], Nothing)
        liftIO $ r3 @=? Just (testData 1)

-- | Run in redis
runInRedis :: forall b. R.Redis b -> IO b
runInRedis a = do
    conn <- R.connect R.defaultConnectInfo
    R.runRedis conn a
    
-- | Test setup
setup :: IO ()
setup = runInRedis $ 
    void $ R.hmset (testPrefix <> ":mark") [("mark", "mark")]

-- | Teardown. Purge all keys with 'allPrefix'
teardown :: IO ()
teardown = runInRedis $ do
    a <- R.keys $ testPrefix <> "*"
    _ <- either undefined R.del a
    return ()
    
-- | Common prefix
testPrefix :: B.ByteString
testPrefix = "piletest"

type TData = (Int, [(B.ByteString, Maybe B.ByteString)], BL.ByteString)

-- | Common data
testData :: 
       Int      -- ^ Param-param 
    -> TData
testData n = 
    (n, [(toBs n, Just $ toBs n)], toLBs n)

-- | Convert int to bytestring
toBs :: Int -> B.ByteString
toBs n = cs . show $ n

-- | Convert int to lazy bytestring
toLBs :: Int -> BL.ByteString
toLBs n = cs . show $ n
