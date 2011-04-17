import Control.Monad (zipWithM)
import System.Environment (getArgs)
import Search.Xapian.Database
import Search.Xapian.Types
import Search.Xapian.Document
import Data.ByteString.Char8 (pack)

main = do
  (dbPath:store:terms) <- getArgs
  (Right db) <- openWritableDatabase CreateOrOpen dbPath
  let doc = foldr (\(pos,term) cont -> addPosting pos (pack term) . cont)
                  id
                  (zip [1..] terms)
            $ document store
  addDocument db doc
  return ()
