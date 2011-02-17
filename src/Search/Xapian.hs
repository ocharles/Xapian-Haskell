{-
Copyright (C) 2011 by Oliver Charles <oliver.g.charles@googlemail.com>

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

-- | The main Haskell Xapian API
module Search.Xapian
       (
         -- * Databases
         openWritableDatabase
       , openDatabase
       , enquire

         -- * Documents
       , newDocument
       , addDocument
       , setDocumentData
       , addPosting

         -- * Stemming
       , stemToDocument

         -- ** Stemming algorithms
       , englishStem

         -- * Queries
       , query
       , parseQuery
       , (<|>), (<&>)
       , describeQuery

         -- * Flags
         -- ** Database modes
       ,  createOrOpenDB
       , createDB
       , createOrOverwriteDB
       , openDB
       ) where

import Data.ByteString.Char8
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Search.Xapian.FFI

data Document = Document !(ForeignPtr XapianDocument)
                deriving (Eq, Show)

data Database = Database !(ForeignPtr XapianDatabase)
                deriving (Eq, Show)

data Enquire = Enquire !(ForeignPtr XapianEnquire)
               deriving (Eq, Show)

data Query = Query !(ForeignPtr XapianEnquire)
           deriving (Eq, Show)

data Stem = Stem !(ForeignPtr XapianStem)
               deriving (Eq, Show)

-- | Create the database if it doesn't exist, otherwise open it for writing
createOrOpenDB :: CreateDBOption
createOrOpenDB = CreateDBOption 1

-- | Create a database. If the database already exists (or cannot be created) an
-- error is raised
createDB :: CreateDBOption
createDB = CreateDBOption 2

-- | Create a new database, overwriting any existing database with the same name
createOrOverwriteDB :: CreateDBOption
createOrOverwriteDB = CreateDBOption 3

-- | Open an existing database for writing. An error will be raised if the
-- database does not already exist
openDB :: CreateDBOption
openDB  = CreateDBOption 4

-- | Martin Porter's 2002 English stemmer
englishStem :: Stem
englishStem = createStem "english"

-- | @stemToDocument stem document text@ adds stemmed posting terms derived from
-- @text@ using the stemming algorith @stem@ to @doc@
stemToDocument :: Stem      -- ^ The stemming algorithm to use
               -> Document  -- ^ The document to add terms to
               -> String    -- ^ The text to stem and index
               -> IO ()
stemToDocument (Stem stem) (Document document) text =
  useAsCString (pack text) $ \ctext ->
  withForeignPtr stem $ \stemPtr ->
  withForeignPtr document $ \documentPtr -> do
    c_xapian_stem_string stemPtr documentPtr ctext

-- | @parseQuery stem query@ will parse a natural language query into an
-- aggregate "Query" value. The stemming algorithm @stem@ will be used on
-- query terms.
parseQuery :: Stem    -- ^ The stemming algorithm to apply to query terms
           -> String  -- ^ The plain text query to parse
           -> Query   -- ^ The parsed aggregate query
parseQuery (Stem stem) query = unsafePerformIO $
  useAsCString (pack query) $ \cQuery ->
  withForeignPtr stem $ \stemPtr -> do
    final_query <- c_xapian_parse_query cQuery stemPtr
    managed <- newForeignPtr c_xapian_query_delete final_query
    return (Query managed)

-- FIXME 'mode' is a bad term here...
-- | @openWritableDatabase filename mode@ will open the database at @filename@
-- with the mode specified by @mode@. If the database could not be opened
-- successfully, a string error message will be stored in the left value.
openWritableDatabase :: String                        -- ^ The path to database
                     -> CreateDBOption
                     -- ^ The mode to open the database with
                     -> IO (Either String Database)
                     -- ^ A handle to the opened database, or a string error
                     -- message
openWritableDatabase filename options =
  useAsCString (pack filename) $ \cFilename ->
  alloca $ \errorPtr -> do
    dbHandle <- c_xapian_writable_db_new cFilename options errorPtr
    if dbHandle == nullPtr
      then do err <- peekCString =<< peek errorPtr
              return (Left err)
      else do managed <- newForeignPtr c_xapian_database_delete dbHandle
              return (Right $ Database managed)

-- | @openDatabase filename@ will open the database at @filename@ in readonly
-- mode. If the database could not be opened, a string error message will be
-- returned in the left value.
openDatabase :: String -- ^ The path to the database
             -> IO (Either String Database)
             -- ^ Either a string error message if the database could not be
             -- opened, or a handle to the open database.
openDatabase filename =
  useAsCString (pack filename) $ \cFilename ->
  alloca $ \errorPtr -> do
    dbHandle <- c_xapian_database_new cFilename errorPtr
    if dbHandle == nullPtr
      then do err <- peekCString =<< peek errorPtr
              return (Left err)
      else do managed <- newForeignPtr c_xapian_database_delete dbHandle
              return (Right $ Database managed)

-- | @addDocument db doc@ will add the document @doc@ to the writable
-- open database @db@.
addDocument :: Database -- ^ The database to add the document to
            -> Document -- ^ The document to add to the database
            -> IO ()
addDocument (Database db) (Document doc) = do
  withForeignPtr doc $ \docptr ->
    withForeignPtr db $ \dbptr ->
    c_xapian_database_add_document dbptr docptr

-- | @newDocument@ will create a new, blank document.
newDocument :: IO (Document) -- ^ The newly created document
newDocument = do
  document <- c_xapian_document_new
  managed <- newForeignPtr c_xapian_document_delete document
  return (Document managed)

-- | @setDocumentData document data@ will set the entire data section
-- of @document@ to @data@, clearing what may have originally been there.
setDocumentData :: Document -- ^ The document who's data to set
                -> String   -- ^ The new contents of the document
                -> IO ()
setDocumentData (Document document) docData =
  useAsCString (pack docData) $ \dat ->
  withForeignPtr document $ \doc_ptr ->
  c_xapian_document_set_data doc_ptr dat

-- | @addPosting document posting pos@ will index the term @posting@ in
-- the document @document@ at position @pos@.
addPosting :: Document -- ^ The document to add a posting to
           -> String   -- ^ The term to index within the document
           -> Int      -- ^ The position of the term within the document
           -> IO ()
addPosting (Document document) term pos =
  useAsCString (pack term) $ \dat ->
  withForeignPtr document $ \doc_ptr ->
  c_xapian_document_add_posting doc_ptr dat pos

-- | @enquire database query offset limit@ will execute @query@ over
-- @database@, limited to @limit@ results and offseting by @offset@
-- initial results. A list of document identifiers are returned.
enquire :: Database   -- ^ The database to perform the query on
        -> Query      -- ^ The query to search for
        -> Int        -- ^ How many results to offset (skip) from the beginning
        -> Int        -- ^ How many results to return
        -> IO ([Int]) -- ^ A list of matching document IDs
enquire (Database database) (Query query) offset limit =
  withForeignPtr database $ \dbptr ->
  withForeignPtr query $ \queryptr -> do
    enquire <- c_xapian_enquire_new dbptr
    let msets = c_xapian_enquire_query enquire queryptr offset limit
    fetchMSets msets
      where fetchMSets msets =
                if c_xapian_msets_valid msets then
                    do mset <- c_xapian_msets_get msets
                       c_xapian_msets_next msets
                       fetchMSets msets >>= \rest -> return (mset:rest)
                else return []

-- | @query term@ will create a verbatim Query object for @term@.
query :: String -- ^ The single query term
      -> Query  -- ^ The resulting query
query term = unsafePerformIO $
  useAsCString (pack term) $ \dat -> do
    query <- c_xapian_query_new dat
    managed <- newForeignPtr c_xapian_query_delete query
    return (Query managed)

-- | @a \<|\> b@ will result in the query @a OR b@
(<|>) :: Query -> Query -> Query
a <|> b = combineQueries a b queryOpOr
  where queryOpOr = 1

-- | @a \<&\> b@ will result in the query @a AND b@
(<&>) :: Query -> Query -> Query
a <&> b = combineQueries a b queryOpAnd
  where queryOpAnd = 0

-- | @describeQuery query@ will result in a plain text description
-- of @query@. Note, this is not really meant for human consumption, but
-- may help for debugging.
describeQuery (Query q) = unsafePerformIO $
  withForeignPtr q $ \query ->
  peekCString $ c_xapian_query_describe query

--
-- Internal functions
--

combineQueries (Query a) (Query b) operator = unsafePerformIO $
  withForeignPtr a $ \queryA ->
  withForeignPtr b $ \queryB -> do
    query <- c_xapian_query_combine operator queryA queryB
    managed <- newForeignPtr c_xapian_query_delete query
    return (Query managed)

createStem language = unsafePerformIO $
  useAsCString (pack language) $ \cLang -> do
    stemHandle <- c_xapian_stem_new cLang
    managed <- newForeignPtr c_xapian_stem_delete stemHandle
    return (Stem managed)
