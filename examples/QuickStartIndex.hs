import Control.Monad (zipWithM)
import System.Environment (getArgs)
import Data.ByteString.Char8 (pack)
import Search.Xapian

main = do
  (dbPath:store:terms) <- getArgs
  (Right db) <- openWritableDatabase CreateOrOpen dbPath
  let doc :: SimpleDocument String
      doc = addPostings (zip [1..] $ map pack terms) $ document store
  addDocument db doc
  return ()
