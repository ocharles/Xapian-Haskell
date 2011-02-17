import System.Environment (getArgs)
import Search.Xapian;

main = do
  (dbPath:terms) <- getArgs
  (Right db) <- openDatabase dbPath
  results <- enquire db (foldl1 (<|>) $ map query terms) 0 10
  (print . ("You may be interested in document #" ++) . show) `mapM` results
  return ()
