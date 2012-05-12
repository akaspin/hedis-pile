{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Solution for caching mandatory data with Redis.
--   
--   In many cases, requires not just pick up or put the data into the cache.
--   As a rule, data are required. 
--   
--   ... check the cache ... if the value is missing, run the calculations ... 
--   put value to cache ... Tedious
--   
--   Solution is quite simple - collapse all of these steps in one operation.

module Database.Redis.Pile (
    
    pile
) where

import Control.Monad.IO.Class (MonadIO)

import qualified Data.ByteString as B
import Data.Binary (Binary(..), encode, decode)
import Data.String.Conversions ((<>), cs)

import qualified Database.Redis as R
import qualified Database.Redis.Tags as RT

-- | Stores computation results in Redis. Computation fires only  
--   if data absent in cache. Of course, to refresh the data, they must first 
--   remove it from the cache.
--   
--   Computation controls everything except prefix and key.
--
--   In background data is stored in Redis as HashSet with two fields: @d@ 
--   for serialized data and @e@ for expect field.
--
--   Time complexity depends on the situation. 
--   
--   * @O(2)@ data exists in cache, expect matches. 
--   
--   * @O(2)@ data exists in cache, expect value is 'Nothing'.
--   
--   * @O(3)@ data exists in cache, but expect value not matches value 
--     in cache.
--   
--   * In all other cases time complexity does not make sense

pile :: forall ma d t . (MonadIO ma,  R.RedisCtx ma (Either t), Binary d) => 
       B.ByteString
            -- ^ Prefix for key and tags.
    -> B.ByteString        
            -- ^ Key in cache. Key will be stored as @prefix:key@
    -> Maybe B.ByteString
            -- ^ Optional expect value. If it matches the value in the cache,
            --   'pile' will return 'Nothing'. This is very useful when data in 
            --   cache can be described with hash. For example, webpage ETag.
    -> (forall mb . (MonadIO mb) => 
            mb (d, B.ByteString, [B.ByteString], Maybe Integer))
            -- ^ Computation that returns data, expect value, tags and 
            --   optional TTL. 
            --   All tags will be stored as @prefix:tag@.
    -> ma (Maybe d)
pile keyPrefix key (Just ev) fn = do
    res <- R.hget (keyPrefix <> ":" <> key) "e"
    case res of
        Right (Just ev') | ev' == ev -> return Nothing
                         | otherwise -> pile keyPrefix key Nothing fn
        _ -> pile keyPrefix key Nothing fn
pile keyPrefix key Nothing fn = do 
    res <- fetchPayload
    case res of
        Nothing -> runFn
        Just res' -> return . Just . decode . cs $ res'  
  where
    withPrefix = keyPrefix <> ":" <> key
    fetchPayload = do
        v <- R.hget withPrefix "d"
        return $ case v of 
            Right (Just v') -> Just v'
            _ -> Nothing
    runFn = do
        -- run and encode data
        (newData, newExpectValue, tags, ttl) <- fn
        let encodedData = cs . encode $ newData
        
        -- Try to get data. 
        maybeInCache <- R.hget withPrefix "d"
        case maybeInCache of
            Right Nothing -> do
                -- no data in cache. store and return
                _ <- R.hmset withPrefix 
                    [("e", newExpectValue), ("d", encodedData)]
                setExpire ttl
                RT.markTags [withPrefix] keyPrefix tags
                return $ Just newData
            Right (Just cachedData) -> 
                -- data in cache. dont set. just return
                return . Just . decode . cs $ cachedData
            _ -> 
                -- some troubles. just return
                return $ Just newData
        
      where
        setExpire Nothing = return ()
        setExpire (Just ke) = do
            _ <- R.expire withPrefix ke
            return ()








