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
       ( XapianM (runXapian)
       , XapianE
       , collectE
       , collectE'
       
         -- * Error types
       , Error (..)

         -- * Indexing
       , Index (..)
         
         -- * Database related types
       , ReadableDatabase (..)
       , WritableDatabase (..)
       , ReadWriteDB (..)
       , ReadOnlyDB (..)
       , InitDBOption (..)
       , packInitDBOption

         -- * Query related types
       , MSet (..)
       , Query (..)
       , QueryPtr
       , QueryRange (..)

         -- * Document related types
       , Document (..)
       , DocumentId (..)
       , Term (..)
       , Wdf
       , ValueNumber
       , Value
       , Pos

         -- * Stemming related types
       , Stemmer (..)
       ) where

import Search.Xapian.Internal.Types
import Search.Xapian.Internal.FFI
import Data.ByteString (ByteString)
import Data.Enumerator (Enumerator, ($$), run_)
import Data.Enumerator.List (consume)

-- * Indexing
-- --------------------------------------------------------------------

class Index d where
    index :: d -> XapianM Document

-- * Database related types
-- --------------------------------------------------------------------

-- | Xapian Enumerator, allows for user defined chunk sizes
type XapianE a b = Int -> Enumerator a XapianM b

-- | like collectE' but with a default chunksize of 4096 (randomly chosen)
collectE :: XapianE a [a] -> XapianM [a]
collectE = collectE' 4096

-- | collects the values from a Xapian Enumerator and returns a list.
--   the chunksize > 0 is freely choosable
collectE' :: Int -> XapianE a [a] -> XapianM [a]
collectE' chunksize xe = run_ $ (xe chunksize) $$ consume

class ReadableDatabase db where
    search          :: db -> Query -> QueryRange -> XapianM (MSet, [Document])
    getDocument     :: db -> DocumentId -> XapianM (Either Error Document)
    suggestSpelling :: db -> ByteString -> Int -> XapianM ByteString

    getSynonyms     :: db -> ByteString -> XapianE ByteString a
    getSynonymKeys  :: db -> ByteString {- prefix -} -> XapianE ByteString a
    getSpellings    :: db -> XapianE (ByteString, Int) a -- spellings and frequencies
    getPostings     :: db -> ByteString -> XapianE (DocumentId,Wdf) a -- when an empty term ist passed all document ids are returned
    getAllTerms     :: db -> XapianE ByteString a

    getDocCount     :: db -> XapianM Integer
    getUUID         :: db -> XapianM ByteString
    getMetadata     :: db -> ByteString -> XapianM ByteString
    getMetadataKeys :: db -> ByteString {- prefix -} -> XapianE ByteString a
    
    termExists      :: db -> ByteString -> XapianM Bool
    termFreq        :: db -> ByteString -> XapianM Int
    collFreq        :: db -> ByteString -> XapianM Int
    wdfMax          :: db -> ByteString -> XapianM Wdf

class WritableDatabase db where
    addDocument       :: db -> Document -> XapianM DocumentId
    delDocumentById   :: db -> DocumentId -> XapianM ()
    delDocumentByTerm :: db -> ByteString -> XapianM ()
    setMetadata       :: db -> ByteString -> ByteString -> XapianM ()
    addSynonym        :: db -> ByteString -> ByteString -> XapianM ()
    delSynonym        :: db -> ByteString -> ByteString -> XapianM ()
    clearSynonyms     :: db -> ByteString -> XapianM ()
    addSpelling       :: db -> ByteString -> Int -> XapianM ()
    delSpelling       :: db -> ByteString -> Int -> XapianM ()


data InitDBOption
    = CreateOrOpen      -- ^ Create the database if it doesn't exist,
                        --   otherwise open it for writing
    | Create            -- ^ Create a database. If the database already exists
                        --   (or cannot be created) an error is raised
    | CreateOrOverwrite -- ^ Create a new database, overwriting any existing
                        --   database with the same name
    | Open              -- ^ Open an existing database for writing. An error
                        --   will be raised if the database does not already
                        --   exist
    deriving (Show, Eq, Ord, Enum)

packInitDBOption :: InitDBOption -> Int
packInitDBOption option =
  case option of
       CreateOrOpen      -> cx_database_DB_CREATE_OR_OPEN
       Create            -> cx_database_DB_CREATE
       CreateOrOverwrite -> cx_database_DB_CREATE_OR_OVERWRITE
       Open              -> cx_database_DB_OPEN

-- * Query related types
-- --------------------------------------------------------------------

newtype MSet = MSet { msetPtr :: MSetPtr }

-- | would YOU expect this when you think of a range?
data QueryRange = QueryRange
    { rangeOffset :: Int
    , rangeSize :: Int
    } deriving (Show)
