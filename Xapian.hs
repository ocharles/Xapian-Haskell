{-# LANGUAGE ForeignFunctionInterface #-}

module Xapian where

import Data.ByteString.Char8
import Foreign
import Foreign.C.String
import Foreign.C.Types

--
-- For testing - here's how it looks to clients
--
testStuff =
  do (Right db) <- openWritableDatabase "test.db" createOrOverwriteDB
     doc <- newDocument
     addPosting doc "red" 1
     addDocument db doc
     doc2 <- newDocument
     addPosting doc2 "red" 1
     addDocument db doc2
     return ()

testQuery =
  do (Right db) <- openDatabase "test.db"
     enquire db (query "red" <&> query "red")

-- Public API (sort of, I need to decide what to export)
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

newtype CreateDBOption = CreateDBOption { unCreateDBOption :: Int }
                         deriving (Show, Eq)

createOrOpenDB      = CreateDBOption 1
createDB            = CreateDBOption 2
createOrOverwriteDB = CreateDBOption 3
openDB              = CreateDBOption 4

englishStem = createStem "english"

createStem language =
  useAsCString (pack language) $ \cLang -> do
    stemHandle <- c_xapian_stem_new cLang
    managed <- newForeignPtr c_xapian_stem_delete stemHandle
    return (Stem managed)

openWritableDatabase filename options =
  useAsCString (pack filename) $ \cFilename ->
  alloca $ \errorPtr -> do
    dbHandle <- c_xapian_writable_db_new cFilename options errorPtr
    if dbHandle == nullPtr
      then do err <- peekCString =<< peek errorPtr
              return (Left err)
      else do managed <- newForeignPtr c_xapian_database_delete dbHandle
              return (Right $ Database managed)

openDatabase filename =
  useAsCString (pack filename) $ \cFilename ->
  alloca $ \errorPtr -> do
    dbHandle <- c_xapian_database_new cFilename errorPtr
    if dbHandle == nullPtr
      then do err <- peekCString =<< peek errorPtr
              return (Left err)
      else do managed <- newForeignPtr c_xapian_database_delete dbHandle
              return (Right $ Database managed)

addDocument (Database db) (Document doc) = do
  withForeignPtr doc $ \docptr ->
    withForeignPtr db $ \dbptr ->
    c_xapian_database_add_document dbptr docptr

newDocument = do
  document <- c_xapian_document_new
  managed <- newForeignPtr c_xapian_document_delete document
  return (Document managed)

setDocumentData (Document document) docData =
  useAsCString (pack docData) $ \dat ->
  withForeignPtr document $ \doc_ptr ->
  c_xapian_document_set_data doc_ptr dat

addPosting (Document document) term pos =
  useAsCString (pack term) $ \dat ->
  withForeignPtr document $ \doc_ptr ->
  c_xapian_document_add_posting doc_ptr dat pos

enquire (Database database) (Query query) =
  withForeignPtr database $ \dbptr ->
  withForeignPtr query $ \queryptr -> do
    enquire <- c_xapian_enquire_new dbptr
    let msets = c_xapian_enquire_query enquire queryptr 0 10
    fetchMSets msets
      where fetchMSets msets =
                if c_xapian_msets_valid msets then
                    do mset <- c_xapian_msets_get msets
                       c_xapian_msets_next msets
                       fetchMSets msets >>= \rest -> return (mset:rest)
                else return []

query term = unsafePerformIO $
  useAsCString (pack term) $ \dat -> do
    query <- c_xapian_query_new dat
    managed <- newForeignPtr c_xapian_query_delete query
    return (Query managed)

combineQueries (Query a) (Query b) operator = unsafePerformIO $
  withForeignPtr a $ \queryA ->
  withForeignPtr b $ \queryB -> do
    query <- c_xapian_query_combine operator queryA queryB
    managed <- newForeignPtr c_xapian_query_delete query
    return (Query managed)

a <|> b = combineQueries a b queryOpOr
  where queryOpOr = 1

a <&> b = combineQueries a b queryOpAnd
  where queryOpAnd = 0

describeQuery (Query q) = unsafePerformIO $
  withForeignPtr q $ \query ->
  peekCString $ c_xapian_query_describe query


-- Private stuff

type XapianDocument = ()
type XapianDatabase = ()
type XapianEnquire = ()
type XapianQuery = ()
type XapianMSetIterator = ()
type XapianStem = ()

foreign import ccall "cxapian.h xapian_writable_db_new"
  c_xapian_writable_db_new :: CString ->
                              CreateDBOption ->
                              Ptr CString ->
                              IO (Ptr XapianDatabase)

foreign import ccall "cxapian.h xapian_writable_db_add_document"
  c_xapian_database_add_document :: Ptr XapianDatabase ->
                                    Ptr XapianDocument ->
                                    IO ()

foreign import ccall "cxapion.h xapian_database_new"
  c_xapian_database_new :: CString ->
                           Ptr CString ->
                           IO (Ptr XapianDatabase)

foreign import ccall "cxapion.h &xapian_database_delete"
  c_xapian_database_delete :: FunPtr (Ptr XapianDatabase -> IO ())

foreign import ccall "cxapian.h xapian_document_new"
  c_xapian_document_new :: IO (Ptr XapianDocument)

foreign import ccall "cxapion.h &xapian_document_delete"
  c_xapian_document_delete :: FunPtr (Ptr XapianDocument -> IO ())

foreign import ccall "cxapian.h xapian_document_set_data"
  c_xapian_document_set_data :: Ptr XapianDocument -> CString -> IO ()

foreign import ccall "cxapian.h xapian_document_add_posting"
  c_xapian_document_add_posting :: Ptr XapianDocument ->
                                   CString ->
                                   Int ->
                                   IO ()

foreign import ccall "cxapian.h xapian_enquire_new"
  c_xapian_enquire_new :: Ptr XapianDatabase ->
                          IO (Ptr XapianEnquire)

foreign import ccall "cxapion.h &xapian_enquire_delete"
  c_xapian_enquire_delete :: FunPtr (Ptr XapianEnquire -> IO ())

foreign import ccall "cxapian.h xapian_query_new"
  c_xapian_query_new :: CString -> IO (Ptr XapianQuery)

foreign import ccall "cxapian.h xapian_query_combine"
  c_xapian_query_combine :: Int ->
                            Ptr XapianQuery ->
                            Ptr XapianQuery ->
                            IO (Ptr XapianQuery)

foreign import ccall "cxapian.h xapian_query_describe"
  c_xapian_query_describe :: Ptr XapianQuery ->
                             CString

foreign import ccall "cxapion.h &xapian_query_delete"
  c_xapian_query_delete :: FunPtr (Ptr XapianQuery -> IO ())

foreign import ccall "cxapian.h xapian_enquire_query"
  c_xapian_enquire_query :: Ptr XapianEnquire -> Ptr XapianQuery ->
                            Int -> Int ->
                            Ptr XapianMSetIterator

foreign import ccall "cxapian.h xapian_msets_valid"
  c_xapian_msets_valid :: Ptr XapianMSetIterator -> Bool

foreign import ccall "cxapian.h xapian_msets_get"
  c_xapian_msets_get :: Ptr XapianMSetIterator -> IO (Int)

foreign import ccall "cxapian.h xapian_msets_next"
  c_xapian_msets_next :: Ptr XapianMSetIterator -> IO ()

foreign import ccall "cxapian.h xapian_stem_new"
  c_xapian_stem_new :: CString -> IO (Ptr XapianStem)

foreign import ccall "cxapian.h &xapian_stem_delete"
  c_xapian_stem_delete :: FunPtr (Ptr XapianStem -> IO())
