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

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import qualified Database.Redis as R
import qualified Database.Redis.Tags as RT

-- | Stores computation results in Redis as hashSet. Computation fires only  
--   if data absent in cache. Of course, to refresh the data, they must first 
--   remove from the cache.
--   
--   Computation controls all that will be stored in the cache. To do this, 
--   except the results of computation, it may return optional @TTL@ in 
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
-- >    r <- pile "mykey" (Just ("etag", "etag")) $  
-- >        return ([("etag", "etag"), ("val", "myval")], Nothing, Nothing)
-- >    liftIO $ print r
-- >    -- Just [("etag", "etag"), ("val", "myval")]
-- >    
-- >    -- once again
-- >    r <- pile "mykey" (Just ("etag", "etag")) $  
-- >        return ([("etag", "etag"), ("val", "myval")], Nothing, Nothing)
-- >    liftIO $ print r
-- >    -- Nothing
-- >    
-- >    -- and again without expect
-- >    r <- pile "mykey" Nothing $  
-- >        return ([("etag", "etag"), ("val", "myval")], Nothing, Nothing)
-- >    liftIO $ print r
-- >    -- Just [("etag", "etag"), ("val", "myval")]
pile :: 
       B.ByteString        
            -- ^ Key in cache.
    -> Maybe (B.ByteString, B.ByteString)
            -- ^ Optional expect field.
    -> IO ([(B.ByteString, B.ByteString)], 
           Maybe Integer, 
           Maybe (B.ByteString, [B.ByteString]))
            -- ^ Computation that returns data and 
            --   optional TTL and tags.
    -> R.Redis (Maybe [(B.ByteString, B.ByteString)])
pile key (Just (ef, ev)) f = do
    e <- R.hget key ef
    case e of
        Right (Just ev') | ev' == ev -> return Nothing
                         | otherwise -> pile key Nothing f
        _ -> pile key Nothing f
pile key Nothing f = do
    d <- R.hgetall key
    case d of
        Right [] -> runF
        Right r -> return $ Just r
        _ -> runF
  where
    runF = do
        (r, ke, t) <- liftIO f
        void $ R.hmset key r
        setExpire ke
        setTags t
        return $ Just r
      where
        setExpire Nothing = return ()
        setExpire (Just ke) = void $ R.expire key ke
        setTags Nothing = return ()
        setTags (Just (p, ts)) = RT.markTags [key] p ts
        
