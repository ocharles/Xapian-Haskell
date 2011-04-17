import Control.Monad (zipWithM)
import System.Environment (getArgs)
import Search.Xapian.Database
import Search.Xapian.Types
import Search.Xapian.Document
import Data.ByteString.Char8 (pack)

main = do
  (dbPath:store:terms) <- getArgs
  (Right db) <- openWritableDatabase CreateOrOpen dbPath
  let doc = addPostings (zip [1..] $ map pack terms) $ document store
  addDocument db doc
  return ()
