{-
Copyright (C) 2011 by Oliver Charles <oliver.g.charles@googlemail.com>
Copyright (C) 2011 by Long Huynh Huu <long.upcase@googlemail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN

-}

module Search.Xapian.Types (
  module Search.Xapian.Types
) where

-- | cereal
import Data.Serialize
import Data.Word
import qualified Data.ByteString as BS
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Search.Xapian.FFI

-- * Sketch the types

-- | t represents the document data
data Document t = Document
  { documentPtr  :: !(ForeignPtr XapianDocument) -- | we dont wanna show
                                                 -- | the documentPtr ...
  , documentData :: t
  } deriving (Eq, Show)

-- | t represents the document data
data Database t = Database !(ForeignPtr XapianDatabase)
  deriving (Eq, Show)

data XQuery = XQuery !(ForeignPtr XapianEnquire)
  deriving (Eq, Show)

-- this pure data type enables us to stay pure
data Query = EmptyQuery | Query BS.ByteString | Or Query Query | And Query Query

data QueryOptions = QueryOptions
  { offset :: Int
  , results :: Int
  }

-- | doc_id == 0 is invalid; what is the range of
newtype DocumentId = DocId { getDocId :: Word32 }

type Term = BS.ByteString
type Pos  = Word32

data Stemmer = Danish | Dutch | English | Finnish | French
             | German | Hungarian | Italian | Norwegian
             | Portuguese | Romanian | Russian | Spanish
             | Swedish | Turkish
             deriving (Show, Eq)


-- * Sketch the functions on documents

newDocument :: Serialize t => t -> IO (Document t)
newDocument = undefined

getDocument :: Serialize t => Database t -> DocumentId -> IO (Document t)
getDocument = undefined

setDocumentData :: Serialize t => Document t -> t -> IO ()
setDocumentData = undefined

addTerm :: Serialize t => Document t -> Term -> IO ()
addTerm = undefined

addTermAt :: Serialize t => Document t -> Term -> Pos -> IO ()
addTermAt = undefined

-- * Sketch the functions on queries

mkQuery :: BS.ByteString -> Query
mkQuery = undefined

queryAll :: [Query] -> Query
queryAll = foldr And EmptyQuery

queryAny :: [Query] -> Query
queryAny = foldr Or EmptyQuery

search :: Serialize t
       => Database
       -> Query
       -> IO [DocumentId]
search = searchWith undefined

searchWith :: Serialize t
           => QueryOptions
           -> Database t
           -> Query
           -> IO [DocumentId]
searchWith = undefined

-- * Sketch the functions on databases

openDatabase :: Serialize t
             => FilePath
             -> IO (Either String (Database t))
openDatabase = openDatabaseWith undefined

openDatabaseWith :: Serialize t
                 => CreateDBOption
                 -> FilePath
                 -> IO (Either String (Database t))
openDatabaseWith = undefined
