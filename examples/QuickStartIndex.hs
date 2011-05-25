import Control.Monad (forM_)
import System.Environment (getArgs)
import Data.ByteString.Char8 (pack)
import Search.Xapian

main = do
  (dbPath:store:terms) <- getArgs
  (Right db) <- openReadWrite CreateOrOpen dbPath
  runXapian $
   do doc <- emptyDocument
      forM_ (zip [1..] terms) $ \(i,term) ->
          addPosting i (pack term) doc
      setData (pack store) doc
      addDocument db doc
