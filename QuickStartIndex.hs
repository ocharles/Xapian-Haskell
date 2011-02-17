import Control.Monad (zipWithM)
import System.Environment (getArgs)
import Xapian;

main = do
  (dbPath:store:terms) <- getArgs
  (Right db) <- openWritableDatabase dbPath createOrOpenDB
  doc <- newDocument
  setDocumentData doc store
  zipWithM (addPosting doc) terms [1..]
  addDocument db doc
  return ()
