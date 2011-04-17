module Search.Xapian.Document where


import Foreign
import qualified Data.ByteString as BS
import Data.Serialize

import Search.Xapian.Types


type Term = BS.ByteString
type Pos  = Word32

newDocument :: Serialize t => t -> IO (Document t)
newDocument = undefined

setDocumentData :: Serialize t => Document t -> t -> IO ()
setDocumentData = undefined

getDocumentData :: Serialize t => Document t -> t -> IO ()
getDocumentData = undefined

addTerm :: Serialize t => Document t -> Term -> IO ()
addTerm = undefined

addTermAt :: Serialize t => Document t -> Term -> Pos -> IO ()
addTermAt = undefined
