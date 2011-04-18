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
         -- * Error
         Error (..)
         
         -- * Databases
       , ReadableDatabase (..)
       , Database (..)
       , WritableDatabase
       , InitDBOption (..)
       , packInitDBOption

         -- * Queries
       , MSet (..)
       , Query (..)
       , QueryPtr
       , QueryRange (..)
       , queryString

         -- * Document
       , Document (..)
       , DocumentId (..)
       , Term (..)
       , Pos
       , document
       ) where

import Control.Applicative
import Data.Either
import Data.Serialize
import Data.Word
import Foreign
import Foreign.C.String
import Search.Xapian.FFI
import Search.Xapian.Internal.Types

-- * database

-- | Error, inspired (blatantly copied) by hdbc
--
-- The main Xapian exception object. As much information as possible is passed from
-- the database through to the application through this object.
--
-- Errors generated in the Haskell layer will have seNativeError set to
-- Nothing.
-- 


class ReadableDatabase db where
  search :: Serialize doc
         => db doc
         -> Query
         -> QueryRange
         -> IO (MSet doc)


  getDocument :: Serialize doc
              => db doc
              -> DocumentId
              -> IO (Either Error (Document doc))

instance ReadableDatabase Database where
  search database@(Database dbFPtr) query (QueryRange off lim) =
   do queryFPtr <- compileQuery query
      withForeignPtr dbFPtr $ \dbPtr ->
          withForeignPtr queryFPtr $ \queryPtr ->
           do enquire <- c_xapian_enquire_new dbPtr
              let msets = c_xapian_enquire_query enquire queryPtr off lim
              MSet . rights <$> fetchMSets msets
    where
      fetchMSets msets =
          if c_xapian_msets_valid msets
             then do mset <- c_xapian_msets_get msets
                     let docId = DocId (fromIntegral mset)
                     doc  <- getDocument database docId
                     c_xapian_msets_next msets
                     rest <- fetchMSets msets
                     return (doc:rest)
             else return []
          

  getDocument (Database database) docId@(DocId id') =
       withForeignPtr database $ \dbPtr ->
       handleError dbPtr $ \docPtr ->
        do docFPtr <- newForeignPtr c_xapian_document_delete docPtr
           eitherDocDat  <- getDocumentData docFPtr
           case eitherDocDat of
                Left err  -> return (Left err)
                Right dat -> do terms <- getDocumentTerms docFPtr
                                return . Right $ Document (Just docId) dat terms
    where
      handleError dbPtr action =
        alloca $ \errorPtr ->
         do handle <- c_xapian_get_document dbPtr (fromIntegral id') errorPtr
            if handle == nullPtr
               then do err <- peekCString =<< peek errorPtr
                       return . Left $ Error (Just DocNotFoundError) err
               else do action handle

instance ReadableDatabase WritableDatabase where
  search (WritableDatabase db) = search db
  getDocument (WritableDatabase db) id' = getDocument db id'

data InitDBOption
  = CreateOrOpen
  | Create
  | CreateOrOverwrite
  | Open
  deriving (Show, Eq, Ord, Enum)

packInitDBOption :: InitDBOption -> Int
packInitDBOption option =
  case option of
       CreateOrOpen      -> 1
       Create            -> 2
       CreateOrOverwrite -> 3
       Open              -> 4

-- * query result

-- | it's a list, not a set
newtype MSet doc = MSet {getMSet :: [Document doc]}

-- | would YOU expect this when you think of a range?
data QueryRange = QueryRange
  { rangeOffset :: Int
  , rangeSize :: Int
  }

-- | t represents the document data
data Document t = Document
  { documentId    :: Maybe DocumentId
  , documentData  :: t
  , documentTerms :: [Term]
  } deriving (Eq, Show)

document :: Serialize t => t -> Document t
document t = Document Nothing t []

-- | doc_id == 0 is invalid; what is the range of
newtype DocumentId = DocId { getDocId :: Word32 }
  deriving (Show, Eq)
