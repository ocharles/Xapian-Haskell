import System.Environment (getArgs)
import Control.Monad (forM_)
import Search.Xapian

main = do
  (dbPath:terms) <- getArgs
  (Right db) <- openReadOnly dbPath
  (_, results) <- runXapian $ search db (queryAny terms) (paging 0 10)
  forM_ results $ \result ->
   do putStr "You may be interested in document #"
      print (docId result)
