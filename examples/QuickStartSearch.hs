import System.Environment (getArgs)
import Control.Monad (forM_)
import Search.Xapian.Database
import Search.Xapian.Types

main = do
  (dbPath:terms) <- getArgs
  (Right db) <- openDatabase dbPath
  MSet results <- searchWith db (foldl1 Or $ map queryString terms) (QueryRange 0 10)
  forM_ (results :: [Document String]) $ \result ->
   do putStr "You may be interested in document#"
      print result
