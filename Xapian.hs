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
  do (Right db) <- openDatabase "test.db" createOrOverwriteDB
     doc <- newDocument
     addPosting doc "A posting" 1
     addDocument db doc
     return ()

-- Public API (sort of, I need to decide what to export)
data Document = Document !(ForeignPtr XapianDocument)
                deriving (Eq, Show)

data Database = Database !(ForeignPtr XapianDatabase)
              | WritableDatabase !(ForeignPtr XapianDatabase)
                deriving (Eq, Show)

data Enquire = Enquire !(ForeignPtr XapianEnquire)
               deriving (Eq, Show)

data Query = Query !(ForeignPtr XapianEnquire)
           deriving (Eq, Show)

newtype DatabaseMode = DatabaseMode { getDatabaseMode :: Int }
                     deriving (Show, Eq)

createOrOpenDB      = Just $ DatabaseMode 1
createDB            = Just $ DatabaseMode 2
createOrOverwriteDB = Just $ DatabaseMode 3
openDB              = Just $ DatabaseMode 4
readOnly            = Nothing

openDatabase filename (Just mode) =
  useAsCString (pack filename) $ \cFilename ->
  alloca $ \errorPtr -> do
    dbHandle <- c_xapian_writable_db_new cFilename mode errorPtr
    if dbHandle == nullPtr
      then do err <- peekCString =<< peek errorPtr
              return (Left err)
      else do managed <- newForeignPtr finalizerFree dbHandle
              return (Right $ WritableDatabase managed)

openDatabase filename Nothing =
  useAsCString (pack filename) $ \cFilename ->
  alloca $ \errorPtr -> do
    dbHandle <- c_xapian_database_new cFilename errorPtr
    if dbHandle == nullPtr
      then do err <- peekCString =<< peek errorPtr
              return (Left err)
      else do managed <- newForeignPtr finalizerFree dbHandle
              return (Right $ Database managed)

addDocument (WritableDatabase db) (Document doc) = do
  withForeignPtr doc $ \docptr ->
    withForeignPtr db $ \dbptr ->
    c_xapian_database_add_document dbptr docptr

newDocument = do
  document <- c_xapian_document_new
  managed <- newForeignPtr finalizerFree document
  return (Document managed)

setDocumentData (Document document) docData =
  useAsCString (pack docData) $ \dat ->
  withForeignPtr document $ \doc_ptr ->
  c_xapian_document_set_data doc_ptr dat

addPosting (Document document) term pos =
  useAsCString (pack term) $ \dat ->
  withForeignPtr document $ \doc_ptr ->
  c_xapian_document_add_posting doc_ptr dat pos

enquire (Database database) =
  withForeignPtr database $ \dbptr -> do
    document <- c_xapian_enquire_new dbptr
    managed <- newForeignPtr finalizerFree document
    return (Enquire managed)

query term = unsafePerformIO $
  useAsCString (pack term) $ \dat -> do
    query <- c_xapian_query_new dat
    managed <- newForeignPtr finalizerFree query
    return (Query managed)

combineQueries (Query a) (Query b) operator = unsafePerformIO $
  withForeignPtr a $ \queryA ->
  withForeignPtr b $ \queryB -> do
    query <- c_xapian_query_combine operator queryA queryB
    managed <- newForeignPtr finalizerFree query
    return (Query managed)

a <|> b = combineQueries a b queryOpOr
  where queryOpOr = 0

describeQuery (Query q) = unsafePerformIO $
  withForeignPtr q $ \query ->
  peekCString $ c_xapian_query_describe query


-- Private stuff

type XapianDocument = ()
type XapianDatabase = ()
type XapianEnquire = ()
type XapianQuery = ()

foreign import ccall "cxapian.h xapian_writable_db_new"
  c_xapian_writable_db_new :: CString ->
                              DatabaseMode ->
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

foreign import ccall "cxapian.h xapian_document_new"
  c_xapian_document_new :: IO (Ptr XapianDocument)

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
