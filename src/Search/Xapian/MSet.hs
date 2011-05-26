module Search.Xapian.MSet
     ( getRanks
     , getWeights
     , getPercents
     ) where

import Foreign
import Control.Monad.Trans (liftIO)

import Search.Xapian.Internal.FFI
import Search.Xapian.Types (MSet (..), XapianM)
import Search.Xapian.Internal.Utils (collect)

getWeights :: MSet -> XapianM [Int]
getWeights = collectMSet (fmap fromIntegral . cx_msetiterator_get_weight)

getRanks :: MSet -> XapianM [Int]
getRanks = collectMSet (fmap fromIntegral . cx_msetiterator_get_rank) 

-- | returns the percents ranging from 0 to 100
getPercents :: MSet -> XapianM [Int]
getPercents = collectMSet (fmap fromIntegral  . cx_msetiterator_get_percent)

collectMSet :: (Ptr CMSetIterator -> IO a) -> MSet -> XapianM [a]
collectMSet retrieve mset =
 do liftIO $ withForeignPtr (msetPtr mset) $ \ptr ->
             do b <- manage =<< cx_mset_begin ptr
                e <- manage =<< cx_mset_end   ptr
                collect cx_msetiterator_next
                        retrieve
                        cx_msetiterator_is_end
                        b e
