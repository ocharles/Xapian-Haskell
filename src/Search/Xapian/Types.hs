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

module Search.Xapian.Types
       (
         -- * Databases
         ReadableDatabase (..)
       , Database
       , WritableDatabase
       , InitDBOption (..)

         -- * Queries
       , Query (..)
       , CompiledQuery
       , QueryOptions (..)

         -- * Document
       , Document (..)
       , DocumentId (..)
       ) where

import Data.Serialize
import Data.Word
import qualified Data.ByteString as BS
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Search.Xapian.FFI

-- * database

class ReadableDatabase db where
  searchWith :: Serialize t
             => QueryOptions
             -> db t
             -> Query
             -> IO [DocumentId]


  getDocument :: Serialize t
              => db t
              -> DocumentId
              -> IO (Document t)

instance ReadableDatabase Database where
  searchWith = undefined
  getDocument = undefined

instance ReadableDatabase WritableDatabase where
  searchWith opts (WritableDatabase db) q = searchWith opts db q
  getDocument (WritableDatabase db) id' = getDocument db id'


data Database t = Database !(ForeignPtr XapianDatabase)
  deriving (Eq, Show)

newtype WritableDatabase t = WritableDatabase (Database t) -- | never export constructor


data InitDBOption
  = Create
  | CreateOrOpen
  | CreateOrOverwrite
  | Open

-- * queries

data Query = EmptyQuery
           | Query BS.ByteString
           | Or Query Query
           | And Query Query
  deriving (Show)

data CompiledQuery = CompiledQuery !(ForeignPtr XapianEnquire)
  deriving (Eq, Show)

data QueryOptions = QueryOptions
  { offset :: Int
  , results :: Int
  }

-- * documents

-- | t represents the document data
data Document t = Document
  { documentPtr  :: !(ForeignPtr XapianDocument) -- | we dont wanna show
                                                 -- | the documentPtr ...
  , documentData :: t
  } deriving (Eq, Show)

-- | doc_id == 0 is invalid; what is the range of
newtype DocumentId = DocId { getDocId :: Word32 }
