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
         -- * Error types
         Error (..)
         
         -- * Database related types
       , ReadableDatabase (..)
       , SimpleDatabase
       , SimpleWritableDatabase
       , Database (..)
       , WritableDatabase
       , InitDBOption (..)
       , packInitDBOption

         -- * Query related types
       , MSet (..)
       , Query (..)
       , QueryPtr
       , QueryRange (..)

         -- * Document related types
       , Prefixable (..)
       , SimpleDocument
       , Fieldless
       , Document (..)
       , DocumentId (..)
       , Term (..)
       , ValueNumber
       , Value
       , Pos

         -- * Stemming related types
       , Stemmer (..)
       ) where

import Data.Either
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Serialize
import Data.Word
import Search.Xapian.Internal.Types

-- * Database related types
-- --------------------------------------------------------------------


class ReadableDatabase db where
  search :: (Serialize dat, Prefixable fields)
         => db fields dat
         -> Query
         -> QueryRange
         -> IO (MSet fields dat)


  getDocument :: (Serialize dat, Prefixable fields)
              => db fields dat
              -> DocumentId
              -> IO (Either Error (Document fields dat))


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

type SimpleDatabase = Database Fieldless
type SimpleWritableDatabase = WritableDatabase Fieldless

packInitDBOption :: InitDBOption -> Int
packInitDBOption option =
  case option of
       CreateOrOpen      -> 1
       Create            -> 2
       CreateOrOverwrite -> 3
       Open              -> 4

-- * Query related types
-- --------------------------------------------------------------------

-- | it's a list, not a set
newtype MSet fields dat = MSet {getMSet :: [Document fields dat]}

-- | would YOU expect this when you think of a range?
data QueryRange = QueryRange
    { rangeOffset :: Int
    , rangeSize :: Int
    } deriving (Show)

-- * Document related types
-- --------------------------------------------------------------------

class Ord fields => Prefixable fields where
    getPrefix   :: fields -> ByteString
    stripPrefix :: fields -> ByteString -> Maybe ByteString

data Fieldless = Fieldless deriving (Show, Ord, Eq)

-- | This instance allows to combine fields
--   (Caution: prefixes may collide)
instance (Prefixable a, Prefixable b) => Prefixable (Either a b) where
  getPrefix (Left a) = getPrefix a
  getPrefix (Right b) = getPrefix b
  stripPrefix (Left a) = stripPrefix a
  stripPrefix (Right b) = stripPrefix b

instance Prefixable Fieldless where
    getPrefix   Fieldless = BS.empty
    stripPrefix Fieldless = const Nothing

-- | A @Document@
data Document fields dat = Document
    { documentId    :: Maybe DocumentId       -- ^ might have an @DocumentId@
                                              --   given by the database
    , documentStem  :: Maybe Stemmer          -- ^ might use a @Stemmer@
    , documentValues :: IntMap Value          -- ^ might have @Values@
                                              --   (metadata) associated
                                              --   with it in order to
                                              --   refine queries
    , documentTerms :: [Term]                 -- ^ has raw @Term@s associated
                                              --   with it
    , documentFields :: Map fields [ByteString]-- ^ has prefixed @Term@s,
                                               --   known as fields
    , documentData  :: dat    -- ^ contains data representing or pointing to
                              --   the original document
    } deriving (Show)

type SimpleDocument = Document Fieldless

-- | doc_id == 0 is invalid; what is the range of
newtype DocumentId = DocId { getDocId :: Word32 }
    deriving (Show, Eq)

type Pos  = Word32


data Term = Term    ByteString     -- ^ a single term
          | Posting Pos ByteString -- ^ a term with information
                                   --   about its position
          | RawText ByteString     -- ^ a text to be parsed as terms
  deriving (Eq, Show)
