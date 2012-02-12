{-# LANGUAGE OverloadedStrings, RankNTypes #-} 

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

import qualified Data.ByteString as B

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (void)

import qualified Database.Redis as R
import qualified Database.Redis.Tags as RT
import Data.Maybe (fromJust)

-- | Stores computation results in Redis as hashSet. Computation fires only  
--   if data absent in cache. Of course, to refresh the data, they must first 
--   remove from the cache.
--   
--   Computation controls all that will be stored in the cache except two 
--   things: key and prefix for keys and tags. To do this, 
--   with the results of computation, it may return optional @TTL@ in 
--   seconds (Redis convention) and tags for key. About tags see 
--   "Database.Redis.Tags".
--   
--   Instead get all data from cache, optional parameter allows simply make 
--   sure that cache holds HashSet with needed field with needed value. If this 
--   is so, 'pile' return 'Nothing'. 
--
-- > conn <- connect defaultConnectInfo
-- > runRedis conn $ do
-- >    -- do it
-- >    r <- pile "myprefix" "mykey" (Just ("etag", "etag")) Nothing $  
-- >        return ([("etag", "etag"), ("val", "myval")], Nothing, [])
-- >    liftIO $ print r
-- >    -- Just [("etag", "etag"), ("val", "myval")]
-- >    
-- >    -- once again
-- >    r <- pile "myprefix" "mykey" (Just ("etag", "etag")) Nothing $  
-- >        return ([("etag", "etag"), ("val", "myval")], Nothing, [])
-- >    liftIO $ print r
-- >    -- Nothing
-- >    
-- >    -- and again without expect
-- >    r <- pile "myprefix" "mykey" Nothing Nothing $  
-- >        return ([("etag", "etag"), ("val", "myval")], Nothing, [])
-- >    liftIO $ print r
-- >    -- Just [("etag", "etag"), ("val", "myval")]
--
--   Time complexity for cached data:
--   
--   * @O(1)@ If expect matches.
--
--   * @O(2)@ If payload field provided
pile :: 
       B.ByteString
            -- ^ Prefix for key and tags.
    -> B.ByteString        
            -- ^ Key in cache. Key will be stored as @prefix:key@
    -> Maybe (B.ByteString, B.ByteString)
            -- ^ Optional expect field.
    -> Maybe B.ByteString
            -- ^ Optional payload field. This reduces time complexity of 
            --   request data from the cache from @O(N)@ to @O(1)@.
            --   Regardless of setting this field, all data from computation
            --   will stored in cache.
    -> (forall m . MonadIO m => m ([(B.ByteString, B.ByteString)], 
           Maybe Integer, 
           [B.ByteString]))
            -- ^ Computation that returns data and 
            --   optional TTL and tags. All tags will be stored as @prefix:tag@.
    -> R.Redis (Maybe [(B.ByteString, B.ByteString)])
pile p key (Just (ef, ev)) payload f = do
    e <- R.hget (p `B.append` ":" `B.append` key) ef
    case e of
        Right (Just ev') | ev' == ev -> return Nothing
                         | otherwise -> pile p key Nothing payload f
        _ -> pile p key Nothing payload f
pile p key Nothing payload f = do
    d <- fetchPayload payload
    case d of
        Nothing -> runF
        Just r -> return $ Just r
  where
    fetchPayload Nothing = do
        v <- R.hgetall withPrefix
        return $ case v of 
            Right [] -> Nothing
            Right r -> Just r
            _ -> Nothing
    fetchPayload (Just plf) = do
        v <- R.hget withPrefix plf
        return $ case v of 
            Right (Just v') -> Just [(plf, v')]
            _ -> Nothing
    
    withPrefix = p `B.append` ":" `B.append` key
    runF = do
        (r, ke, t) <- liftIO f
        void $ R.hmset withPrefix r
        setExpire ke
        RT.markTags [withPrefix] p t
        return $ case payload of
            Nothing -> Just r
            Just pl -> Just [(pl, fromJust $ lookup pl r)]
      where
        setExpire Nothing = return ()
        setExpire (Just ke) = void $ R.expire withPrefix ke
        
