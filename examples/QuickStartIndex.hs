import Control.Monad (zipWithM)
import System.Environment (getArgs)
import Data.ByteString.Char8 (pack)
import Search.Xapian

main = do
  (dbPath:store:terms) <- getArgs
  (Right db) <- openWritableDatabase CreateOrOpen dbPath
  let doc :: SimpleDocument String
      doc = addPostings ((map pack terms) `zip` [1..]) $ addData store $ emptyDocument
  addDocument db doc
  return ()
