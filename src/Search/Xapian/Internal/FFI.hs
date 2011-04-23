{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Search.Xapian.Internal.FFI where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import qualified Data.ByteString as BS

-- Document
-- ---------------------------------------------------------

data CDocument

foreign import ccall "document_new"
    cx_document_new :: IO (Ptr CDocument)

foreign import ccall "document_copy"
    cx_document_copy :: Ptr CDocument -> IO (Ptr CDocument)

foreign import ccall "document_delete"
    cx_document_delete :: Ptr CDocument -> IO ()

foreign import ccall "document_get_value"
    cx_document_get_value :: Ptr CDocument -> IO CString

foreign import ccall "document_add_value"
    cx_document_add_value :: Ptr CDocument -> CUInt -> CString -> IO ()

foreign import ccall "document_remove_value"
    cx_document_remove_value :: Ptr CDocument -> CUInt -> IO ()

foreign import ccall "document_clear_values"
    cx_document_clear_values :: Ptr CDocument -> IO ()

foreign import ccall "document_get_data"
    cx_document_get_data :: Ptr CDocument -> IO CString

foreign import ccall "document_set_data"
    cx_document_set_data :: Ptr CDocument -> CString -> IO ()

foreign import ccall "document_add_posting"
    cx_document_add_posting :: Ptr CDocument
                            -> CString       -- term
                            -> CUInt         -- term position
                            -> CUInt         -- within-document frequency increment
                            -> IO ()

foreign import ccall "document_add_term"
    cx_document_add_term :: Ptr CDocument
                         -> CString       -- term
                         -> CUInt         -- wdf increment
                         -> IO ()

foreign import ccall "document_add_boolean_term"
    cx_document_add_boolean_term :: Ptr CDocument
                                 -> CString       -- term
                                 -> IO ()

foreign import ccall "document_remove_posting"
    cx_document_remove_posting :: Ptr CDocument
                               -> CString       -- term
                               -> CUInt         -- term position
                               -> CUInt         -- wdf decrement
                               -> IO ()

foreign import ccall "document_remove_term"
    cx_document_remove_term :: Ptr CDocument -> CString -> IO ()

foreign import ccall "document_clear_terms"
    cx_document_clear_terms :: Ptr CDocument -> IO ()

foreign import ccall "document_termlist_count"
    cx_document_termlist_count :: Ptr CDocument -> IO CUInt

foreign import ccall "document_termlist_begin"
    cx_document_termlist_begin :: Ptr CDocument -> IO (Ptr CTermIterator)

foreign import ccall "document_termlist_end"
    cx_document_termlist_end :: Ptr CDocument -> IO (Ptr CTermIterator)
  
foreign import ccall "document_values_count"
    cx_document_values_count :: Ptr CDocument -> IO CUInt

foreign import ccall "document_values_begin"
    cx_document_values_begin :: Ptr CDocument -> IO (Ptr CValueIterator)

foreign import ccall "document_values_end"
    cx_document_values_end :: Ptr CDocument -> IO (Ptr CValueIterator)

foreign import ccall "document_get_docid"
    cx_document_get_docid :: Ptr CDocument -> IO CUInt

foreign import ccall "document_get_description"
    cx_document_get_description :: Ptr CDocument -> IO CString

-- Query
-- ---------------------------------------------------------

data CQuery
type Op = Int

foreign import ccall "OP_AND" cx_query_OP_AND :: Int
foreign import ccall "OP_OR" cx_query_OP_OR :: Int
foreign import ccall "OP_AND_NOT" cx_query_OP_AND_NOT :: Int
foreign import ccall "OP_XOR" cx_query_OP_XOR :: Int
foreign import ccall "OP_AND_MAYBE" cx_query_OP_AND_MAYBE :: Int
foreign import ccall "OP_FILTER" cx_query_OP_FILTER :: Int
foreign import ccall "OP_NEAR" cx_query_OP_NEAR :: Int
foreign import ccall "OP_PHRASE" cx_query_OP_PHRASE :: Int
foreign import ccall "OP_VALUE_RANGE" cx_query_OP_VALUE_RANGE :: Int
foreign import ccall "OP_SCALE_WEIGHT" cx_query_OP_SCALE_WEIGHT :: Int
foreign import ccall "OP_ELITE_SET" cx_query_OP_ELITE_SET :: Int
foreign import ccall "OP_VALUE_GE" cx_query_OP_VALUE_GE :: Int
foreign import ccall "OP_VALUE_LE" cx_query_OP_VALUE_LE :: Int
foreign import ccall "OP_SYNONYM" cx_query_OP_SYNONYM :: Int

foreign import ccall "query_new"
    cx_query_new :: IO (Ptr CQuery)

foreign import ccall "query_copy"
    cx_query_copy :: Ptr CQuery -> IO (Ptr CQuery)
  
foreign import ccall "query_delete"
    cx_query_delete :: Ptr CQuery -> IO ()

foreign import ccall "query_new_0"
    cx_query_new_0 :: CString -> CUInt -> CUInt -> IO (Ptr CQuery)

foreign import ccall "query_new_1"
    cx_query_new_1 :: Op -> Ptr CQuery -> Ptr CQuery -> IO (Ptr CQuery)

foreign import ccall "query_new_2"
    cx_query_new_2 :: Op -> CString -> CString -> IO (Ptr CQuery)

--foreign import ccall "query_new_3"
--    cx_query_new_3 :: Op -> CString -> CString -> IO (Ptr CQuery)

foreign import ccall "query_new_4"
    cx_query_new_4 :: Op -> Ptr CQuery -> CDouble -> IO (Ptr CQuery)

--foreign import ccall "query_new_5"
--    cx_query_new_5 :: Op -> CString -> CString -> IO (Ptr CQuery)

foreign import ccall "query_new_6"
    cx_query_new_6 :: Op -> CUInt -> CString -> IO (Ptr CQuery)

-- foreign import ccall "query_new_7"

foreign import ccall "query_match_all"
    cx_query_match_all :: IO (Ptr CQuery)

foreign import ccall "query_match_nothing"
    cx_query_match_nothing :: IO (Ptr CQuery)

foreign import ccall "query_get_length"
    cx_query_get_length :: Ptr CQuery -> IO CUInt

foreign import ccall "query_empty"
    cx_query_empty :: Ptr CQuery -> IO Bool
 
foreign import ccall "query_serialise"
    cx_query_serialise :: Ptr CQuery -> IO CString

foreign import ccall "query_get_terms_begin"
    cx_query_get_terms_begin :: Ptr CQuery -> IO (Ptr CTermIterator)

foreign import ccall "query_get_terms_end"
    cx_query_get_terms_end :: Ptr CQuery -> IO (Ptr CTermIterator)

foreign import ccall "query_get_description"
    cx_query_get_description :: Ptr CQuery -> IO CString


-- PositionIterator
-- ---------------------------------------------------------

data CPositionIterator

foreign import ccall "positioniterator_new"
    cx_positioniterator_new :: IO (Ptr CPositionIterator)

foreign import ccall "positioniterator_copy"
    cx_positioniterator_copy :: Ptr CPositionIterator
                             -> IO (Ptr CPositionIterator)

foreign import ccall "positioniterator_delete"
    cx_positioniterator_delete :: Ptr CPositionIterator -> IO ()

foreign import ccall "positioniterator_skip_to"
    cx_positioniterator_skip_to :: Ptr CPositionIterator -> CUInt -> IO ()

foreign import ccall "positioniterator_get_description"
    cx_positioniterator_get_description :: Ptr CPositionIterator
                                        -> IO CString

-- TermIterator
-- ---------------------------------------------------------

data CTermIterator

foreign import ccall "termiterator_new"
    cx_termiterator_new :: IO (Ptr CTermIterator)

foreign import ccall "termiterator_copy"
    cx_termiterator_copy :: Ptr CTermIterator -> IO (Ptr CTermIterator)

foreign import ccall "termiterator_delete"
    cx_termiterator_delete :: Ptr CTermIterator -> IO ()

foreign import ccall "termiterator_next"
    cx_termiterator_next :: Ptr CTermIterator -> IO ()

foreign import ccall "termiterator_is_end"
    cx_termiterator_is_end :: Ptr CTermIterator -> Ptr CTermIterator
                           -> IO Bool

foreign import ccall "termiterator_get"
    cx_termiterator_get :: Ptr CTermIterator -> IO CString

foreign import ccall "termiterator_skip_to"
    cx_termiterator_skip_to :: Ptr CTermIterator -> CString -> IO ()

foreign import ccall "termiterator_get_wdf"
    cx_termiterator_get_wdf :: Ptr CTermIterator -> IO CUInt

foreign import ccall "termiterator_get_termfreq"
    cx_termiterator_get_termfreq :: Ptr CTermIterator -> IO CUInt

foreign import ccall "termiterator_positionlist_count"
    cx_termiterator_positionlist_count :: Ptr CTermIterator -> IO CUInt

foreign import ccall "termiterator_positionlist_begin"
    cx_termiterator_positionlist_begin :: Ptr CTermIterator
                                       -> IO (Ptr CPositionIterator)

foreign import ccall "termiterator_positionlist_end"
    cx_termiterator_positionlist_end :: Ptr CTermIterator
                                     -> IO (Ptr CPositionIterator)

-- ValueIterator
-- ---------------------------------------------------------

data CValueIterator

foreign import ccall "valueiterator_new"
    cx_valueiterator_new :: IO (Ptr CValueIterator)

foreign import ccall "valueiterator_copy"
    cx_valueiterator_copy :: Ptr CValueIterator -> IO (Ptr CValueIterator)

foreign import ccall "valueiterator_delete"
    cx_valueiterator_delete :: Ptr CValueIterator -> IO ()

foreign import ccall "valueiterator_get"
    cx_valueiterator_get :: Ptr CValueIterator -> IO CString

foreign import ccall "valueiterator_next"
    cx_valueiterator_next :: Ptr CValueIterator -> IO ()

foreign import ccall "valueiterator_get_docid"
    cx_valueiterator_get_docid :: Ptr CValueIterator -> IO CUInt

foreign import ccall "valueiterator_get_valueno"
    cx_valueiterator_get_valueno :: Ptr CValueIterator -> IO CUInt

foreign import ccall "valueiterator_skip_to"
    cx_valueiterator_skip_to :: Ptr CValueIterator
                             -> CUInt -- docid or slot
                             -> IO ()

foreign import ccall "valueiterator_check"
    cx_valueiterator_check :: Ptr CValueIterator
                           -> CUInt              -- docid
                           -> IO Bool

foreign import ccall "valueiterator_get_description"
    cx_valueiterator_get_description :: Ptr CValueIterator -> IO CString
