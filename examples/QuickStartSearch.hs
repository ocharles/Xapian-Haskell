import System.Environment (getArgs)
import Control.Monad (forM_)
import Search.Xapian

main = do
  (dbPath:terms) <- getArgs
  (Right db) <- openDatabase dbPath
  MSet results <- search db (queryAny terms) (QueryRange 0 10)
  forM_ (results :: [Document String]) $ \result ->
   do putStr "You may be interested in document#"
      print result
