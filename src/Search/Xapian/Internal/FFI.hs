{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Search.Xapian.Internal.FFI where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Data.ByteString
import Data.ByteString.Unsafe


class Manageable a where
    manage :: Ptr a -> IO (ForeignPtr a)

type CBool = CInt

-- Helper
-- ---------------------------------------------------------

data StdString -- C++ string
type CCString = ForeignPtr StdString

instance Manageable StdString where
    manage = newForeignPtr ccstring_delete

foreign import ccall safe
    ccstring_from_cstring :: CString -> CUInt -> IO (Ptr StdString)

toCCString :: ByteString -> IO CCString
toCCString bs =
    unsafeUseAsCStringLen bs $ \(cstring,len) ->
    ccstring_from_cstring cstring (fromIntegral len) >>= manage

foreign import ccall safe
    ccstring_to_cstring :: Ptr StdString -> IO CString

foreign import ccall safe
    ccstring_length :: Ptr StdString -> IO CUInt

foreign import ccall safe "&"
    ccstring_delete :: FunPtr (Ptr StdString -> IO ())

fromCCString :: CCString -> IO ByteString
fromCCString mccstring = withForeignPtr mccstring $ \ccstring ->
 do cstring <- ccstring_to_cstring ccstring
    len     <- ccstring_length     ccstring
    unsafePackCStringLen (cstring, fromIntegral len)

-- Generic Database
-- ---------------------------------------------------------

data CDatabase
type DatabasePtr = ForeignPtr CDatabase

instance Manageable CDatabase where
    manage = newForeignPtr cx_database_delete

-- Read-only databases

foreign import ccall unsafe "database_new"
    cx_database_new :: IO (Ptr CDatabase)

foreign import ccall unsafe "database_new_from_path"
    cx_database_new_from_path
        :: CString            -- ^ database path
        -> Ptr CString        -- ^ string for error messages to be filled in
        -> IO (Ptr CDatabase)

foreign import ccall unsafe "database_copy"
    cx_database_copy :: Ptr CDatabase -> IO (Ptr CDatabase)

foreign import ccall unsafe "&database_delete"
    cx_database_delete :: FunPtr(Ptr CDatabase -> IO ())

foreign import ccall unsafe "database_add_database"
    cx_database_add_database :: Ptr CDatabase -> Ptr CDatabase -> IO ()

foreign import ccall unsafe "database_reopen"
    cx_database_reopen :: Ptr CDatabase -> IO ()

foreign import ccall unsafe "database_close"
    cx_database_close :: Ptr CDatabase -> IO ()

foreign import ccall unsafe "database_get_description"
    cx_database_get_description :: Ptr CDatabase -> IO CString

foreign import ccall unsafe "database_postlist_begin"
    cx_database_postlist_begin :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CPostingIterator)

foreign import ccall unsafe "database_postlist_end"
    cx_database_postlist_end :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CPostingIterator)

foreign import ccall unsafe "database_termlist_begin"
    cx_database_termlist_begin :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_termlist_end"
    cx_database_termlist_end :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_has_positions"
    cx_database_has_positions :: Ptr CDatabase -> IO CBool

foreign import ccall unsafe "database_positionlist_begin"
    cx_database_positionlist_begin :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CPositionIterator)

foreign import ccall unsafe "database_positionlist_end"
    cx_database_positionlist_end :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CPositionIterator)

foreign import ccall unsafe "database_allterms_begin"
    cx_database_allterms_begin :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_allterms_end"
    cx_database_allterms_end:: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_allterms_with_prefix_begin"
    cx_database_allterms_with_prefix_begin
        :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_allterms_with_prefix_end"
    cx_database_allterms_with_prefix_end
        :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_get_doccount"
    cx_database_get_doccount :: Ptr CDatabase -> IO Word32

foreign import ccall unsafe "database_get_lastdocid"
    cx_database_get_lastdocid :: Ptr CDatabase -> IO Word32

foreign import ccall unsafe "database_get_avlength"
    cx_database_get_avlength :: Ptr CDatabase -> IO Double

foreign import ccall unsafe "database_get_termfreq"
    cx_database_get_termfreq :: Ptr CDatabase -> Ptr StdString -> IO Word32

foreign import ccall "database_term_exists"
    cx_database_term_exists :: Ptr CDatabase -> Ptr StdString -> IO CBool

foreign import ccall "database_get_collection_freq"
    cx_database_get_collection_freq :: Ptr CDatabase -> Ptr StdString -> IO Word32

foreign import ccall unsafe "database_get_value_freq"
    cx_database_get_value_freq :: Ptr CDatabase -> Word32 -> IO Word32

foreign import ccall unsafe "database_get_value_lower_bound"
    cx_database_get_value_lower_bound :: Ptr CDatabase -> Word32 -> IO Word32

foreign import ccall unsafe "database_get_doclength_lower_bound"
    cx_database_get_doclength_lower_bound :: Ptr CDatabase -> Word32 -> IO Word32

foreign import ccall unsafe "database_get_wdf_upper_bound"
    cx_database_get_wdf_upper_bound :: Ptr CDatabase -> Ptr StdString -> IO Word32

foreign import ccall unsafe "database_valuestream_begin"
    cx_database_valuestream_begin
        :: Ptr CDatabase -> Word32 -> IO (Ptr CValueIterator)

foreign import ccall unsafe "database_valuestream_end"
    cx_database_valuestream_end
        :: Ptr CDatabase -> Word32 -> IO (Ptr CValueIterator)

foreign import ccall unsafe "database_get_doclength"
    cx_database_get_doclength :: Ptr CDatabase -> Word32 -> Word32

foreign import ccall unsafe "database_keep_alive"
    cx_database_keep_alive :: Ptr CDatabase -> IO ()

foreign import ccall unsafe "database_get_document"
    cx_database_get_document
        :: Ptr CDatabase
        -> Word32               -- ^ document id
        -> Ptr CString         -- ^ string for error messages
        -> IO (Ptr CDocument)

foreign import ccall unsafe "database_get_spelling_suggestion"
    cx_database_get_spelling_suggestion
        :: Ptr CDatabase
        -> Ptr StdString      -- ^ word
        -> Word32            -- ^ maximum edit distance
        -> IO (Ptr StdString) -- ^ suggested word

foreign import ccall unsafe "database_spellings_begin"
    cx_database_spellings_begin :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_spellings_end"
    cx_database_spellings_end :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_synonyms_begin"
    cx_database_synonyms_begin :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_synonyms_end"
    cx_database_synonyms_end :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_synonym_keys_begin"
    cx_database_synonym_keys_begin
        :: Ptr CDatabase
        -> Ptr StdString           -- ^ prefix
        -> IO (Ptr CTermIterator)

-- | see @cx_database_synonym_keys_begin@
foreign import ccall unsafe "database_synonym_keys_end"
    cx_database_synonym_keys_end
        :: Ptr CDatabase
        -> Ptr StdString
        -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_get_metadata"
    cx_database_get_metadata
        :: Ptr CDatabase
        -> Ptr StdString      -- ^ key
        -> IO (Ptr StdString) -- ^ value

foreign import ccall unsafe "database_metadata_keys_begin"
    cx_database_metadata_keys_begin :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_metadata_keys_end"
    cx_database_metadata_keys_end :: Ptr CDatabase -> Ptr StdString -> IO (Ptr CTermIterator)

foreign import ccall unsafe "database_get_uuid"
    cx_database_get_uuid :: Ptr CDatabase -> IO (Ptr StdString)

-- Writable database
-- ---------------------------------------------------------

type DbAction = Int

foreign import ccall unsafe "DB_CREATE_OR_OPEN"
  cx_database_DB_CREATE_OR_OPEN :: DbAction

foreign import ccall unsafe "DB_CREATE"
  cx_database_DB_CREATE :: DbAction

foreign import ccall unsafe "DB_CREATE_OR_OVERWRITE"
  cx_database_DB_CREATE_OR_OVERWRITE :: DbAction

foreign import ccall unsafe "DB_OPEN"
  cx_database_DB_OPEN :: DbAction


data CWritableDatabase
type WritableDatabasePtr = ForeignPtr CWritableDatabase

instance Manageable CWritableDatabase where
    manage = newForeignPtr cx_database_writable_delete


foreign import ccall unsafe "database_writable_new"
    cx_database_writable_new :: IO (Ptr CWritableDatabase)

foreign import ccall unsafe "database_writable_new_from_path"
    cx_database_writable_new_from_path
        :: CString            -- ^ database path
        -> DbAction           -- ^ how to open the database
        -> Ptr CString        -- ^ string for error messages to be filled in
        -> IO (Ptr CWritableDatabase)

foreign import ccall unsafe "database_writable_copy"
    cx_database_writable_copy :: Ptr CWritableDatabase -> IO (Ptr CWritableDatabase)

foreign import ccall unsafe "&database_writable_delete"
    cx_database_writable_delete :: FunPtr (Ptr CWritableDatabase -> IO ())

foreign import ccall unsafe "database_commit"
    cx_database_commit :: Ptr CWritableDatabase -> IO ()

foreign import ccall unsafe "database_begin_transaction"
    cx_database_begin_transaction
        :: Ptr CWritableDatabase
        -> CBool          -- ^ flushed
        -> IO ()

foreign import ccall unsafe "database_commit_transaction"
    cx_database_commit_transaction :: Ptr CWritableDatabase -> IO ()

foreign import ccall unsafe "database_cancel_transaction"
    cx_database_cancel_transaction :: Ptr CWritableDatabase -> IO ()

foreign import ccall unsafe "database_add_document"
    cx_database_add_document :: Ptr CWritableDatabase -> Ptr CDocument -> IO Word32

foreign import ccall unsafe "database_delete_document_by_id"
    cx_database_delete_document_by_id :: Ptr CWritableDatabase -> Word32 -> IO ()

foreign import ccall unsafe "database_delete_document_by_term"
    cx_database_delete_document_by_term :: Ptr CWritableDatabase -> Ptr StdString -> IO ()

foreign import ccall unsafe "database_replace_document"
    cx_database_replace_document :: Ptr CWritableDatabase -> Word32 -> Ptr CDocument -> IO ()

foreign import ccall unsafe "database_add_spelling"
    cx_database_add_spelling
        :: Ptr CWritableDatabase
        -> Ptr StdString  -- ^ word
        -> Word32         -- ^ frequency increase
        -> IO ()

foreign import ccall unsafe "database_remove_spelling"
    cx_database_remove_spelling
        :: Ptr CWritableDatabase
        -> Ptr StdString  -- ^ word
        -> Word32         -- ^ frequency decrease
        -> IO ()

foreign import ccall unsafe "database_add_synonym"
    cx_database_add_synonym
        :: Ptr CWritableDatabase
        -> Ptr StdString  -- ^ term
        -> Ptr StdString  -- ^ synonym
        -> IO ()

foreign import ccall unsafe "database_remove_synonym"
    cx_database_remove_synonym
        :: Ptr CWritableDatabase
        -> Ptr StdString  -- ^ term
        -> Ptr StdString  -- ^ synonym
        -> IO ()

foreign import ccall unsafe "database_clear_synonyms"
    cx_database_clear_synonyms :: Ptr CWritableDatabase -> Ptr StdString -> IO ()

foreign import ccall unsafe "database_set_metadata"
    cx_database_set_metadata
        :: Ptr CWritableDatabase
        -> Ptr StdString  -- ^ key
        -> Ptr StdString  -- ^ value
        -> IO ()

foreign import ccall unsafe "database_writable_get_description"
    cx_database_writable_get_description :: Ptr CWritableDatabase -> IO CString

-- Document
-- ---------------------------------------------------------

data CDocument
type DocumentPtr = ForeignPtr CDocument

instance Manageable CDocument where
    manage = newForeignPtr cx_document_delete

foreign import ccall unsafe "document_new"
    cx_document_new :: IO (Ptr CDocument)

foreign import ccall unsafe "document_copy"
    cx_document_copy :: Ptr CDocument -> IO (Ptr CDocument)

foreign import ccall unsafe "&document_delete"
    cx_document_delete :: FunPtr (Ptr CDocument -> IO ())

foreign import ccall unsafe "document_get_value"
    cx_document_get_value :: Ptr CDocument -> Word32 -> IO CString

foreign import ccall unsafe "document_add_value"
    cx_document_add_value :: Ptr CDocument -> Word32 -> CString -> IO ()

foreign import ccall unsafe "document_remove_value"
    cx_document_remove_value :: Ptr CDocument -> Word32 -> IO ()

foreign import ccall unsafe "document_clear_values"
    cx_document_clear_values :: Ptr CDocument -> IO ()

foreign import ccall unsafe "document_get_data"
    cx_document_get_data :: Ptr CDocument -> IO (Ptr StdString)

foreign import ccall unsafe "document_set_data"
    cx_document_set_data :: Ptr CDocument -> Ptr StdString -> IO ()

foreign import ccall unsafe "document_add_posting"
    cx_document_add_posting :: Ptr CDocument
                            -> CString       -- term
                            -> Word32         -- term position
                            -> Word32         -- within-document frequency increment
                            -> IO ()

foreign import ccall unsafe "document_add_term"
    cx_document_add_term :: Ptr CDocument
                         -> CString       -- term
                         -> Word32         -- wdf increment
                         -> IO ()

foreign import ccall unsafe "document_add_boolean_term"
    cx_document_add_boolean_term :: Ptr CDocument
                                 -> CString       -- term
                                 -> IO ()

foreign import ccall unsafe "document_remove_posting"
    cx_document_remove_posting :: Ptr CDocument
                               -> CString       -- term
                               -> Word32         -- term position
                               -> Word32         -- wdf decrement
                               -> IO ()

foreign import ccall unsafe "document_remove_term"
    cx_document_remove_term :: Ptr CDocument -> CString -> IO ()

foreign import ccall unsafe "document_clear_terms"
    cx_document_clear_terms :: Ptr CDocument -> IO ()

foreign import ccall unsafe "document_termlist_count"
    cx_document_termlist_count :: Ptr CDocument -> IO Word32

foreign import ccall unsafe "document_termlist_begin"
    cx_document_termlist_begin :: Ptr CDocument -> IO (Ptr CTermIterator)

foreign import ccall unsafe "document_termlist_end"
    cx_document_termlist_end :: Ptr CDocument -> IO (Ptr CTermIterator)
  
foreign import ccall unsafe "document_values_count"
    cx_document_values_count :: Ptr CDocument -> IO Word32

foreign import ccall unsafe "document_values_begin"
    cx_document_values_begin :: Ptr CDocument -> IO (Ptr CValueIterator)

foreign import ccall unsafe "document_values_end"
    cx_document_values_end :: Ptr CDocument -> IO (Ptr CValueIterator)

foreign import ccall unsafe "document_get_docid"
    cx_document_get_docid :: Ptr CDocument -> IO Word32

foreign import ccall unsafe "document_get_description"
    cx_document_get_description :: Ptr CDocument -> IO CString


-- Enquire
-- ---------------------------------------------------------

data CEnquire
type EnquirePtr = ForeignPtr CEnquire

instance Manageable CEnquire where
    manage = newForeignPtr cx_enquire_delete

foreign import ccall unsafe "enquire_new"
    cx_enquire_new :: Ptr CDatabase -> IO (Ptr CEnquire)

foreign import ccall unsafe "&enquire_delete"
    cx_enquire_delete :: FunPtr (Ptr CEnquire -> IO ())

foreign import ccall unsafe "enquire_set_query"
    cx_enquire_set_query
        :: Ptr CEnquire
        -> Ptr CQuery
        -> Word32          -- ^ query length
        -> IO ()

foreign import ccall unsafe "enquire_get_mset"
    cx_enquire_get_mset :: Ptr CEnquire
                        -> Word32        -- ^ skip n items
                        -> Word32        -- ^ maximum amount of items
                        -> IO (Ptr CMSet)

-- MSet
-- ---------------------------------------------------------

data CMSet
type MSetPtr = ForeignPtr CMSet

instance Manageable CMSet where
    manage = newForeignPtr cx_mset_delete

foreign import ccall unsafe "mset_new"
    cx_mset_new :: IO (Ptr CMSet)

foreign import ccall unsafe "mset_copy"
    cx_mset_copy :: Ptr CMSet -> IO (Ptr CMSet)

foreign import ccall unsafe "&mset_delete"
    cx_mset_delete :: FunPtr (Ptr CMSet -> IO ())

foreign import ccall unsafe "mset_fetch_all"
    cx_mset_fetch_all :: Ptr CMSet -> IO ()

foreign import ccall unsafe "mset_fetch_one"
    cx_mset_fetch_one :: Ptr CMSet -> Ptr CMSetIterator -> IO ()

foreign import ccall unsafe "mset_fetch_many"
    cx_mset_fetch_many
        :: Ptr CMSet
        -> Ptr CMSetIterator   -- ^ begin
        -> Ptr CMSetIterator   -- ^ end

foreign import ccall unsafe "mset_convert_weight_to_percent"
    cx_mset_convert_weight_to_percent :: Ptr CMSet -> Double -> IO Int

foreign import ccall unsafe "mset_convert_document_to_percent"
    cx_mset_convert_document_to_percent
        :: Ptr CMSet -> Ptr CMSetIterator -> IO Int

foreign import ccall unsafe "mset_get_termfreq"
    cx_mset_get_termfreq :: Ptr CMSet -> CString -> IO Word32

foreign import ccall unsafe "mset_get_termweight"
    cx_mset_get_termweight :: Ptr CMSet -> CString -> IO Double

foreign import ccall unsafe "mset_get_firstitem"
    cx_mset_get_firstitem :: Ptr CMSet -> IO Word32

foreign import ccall unsafe "mset_get_matches_lower_bound"
    cx_mset_get_matches_lower_bound :: Ptr CMSet -> IO Word32

foreign import ccall unsafe "mset_get_matches_estimated"
    cx_mset_get_matches_estimated :: Ptr CMSet -> IO Word32

foreign import ccall unsafe "mset_get_matches_upper_bound"
    cx_mset_get_matches_upper_bound :: Ptr CMSet -> IO Word32

foreign import ccall unsafe "mset_get_uncollapsed_matches_lower_bound"
    cx_mset_get_uncollapsed_matches_lower_bound :: Ptr CMSet -> IO Word32  

foreign import ccall unsafe "mset_get_uncollapsed_matches_estimated"
    cx_mset_get_uncollapsed_matches_estimated :: Ptr CMSet -> IO Word32

foreign import ccall unsafe "mset_get_uncollapsed_matches_upper_bound"
    cx_mset_get_uncollapsed_matches_upper_bound :: Ptr CMSet -> IO Word32

foreign import ccall unsafe "mset_get_max_possible"
    cx_mset_get_max_possible :: Ptr CMSet -> IO Double

foreign import ccall unsafe "mset_get_max_attained"
    cx_mset_get_max_attained :: Ptr CMSet -> IO Double

foreign import ccall unsafe "mset_size"
    cx_mset_size :: Ptr CMSet -> IO Word32

foreign import ccall unsafe "mset_max_size"
    cx_mset_max_size :: Ptr CMSet -> IO Word32

foreign import ccall unsafe "mset_empty"
    cx_mset_empty :: Ptr CMSet -> IO CBool

foreign import ccall unsafe "mset_swap"
    cx_mset_swap :: Ptr CMSet -> Ptr CMSet -> IO ()

foreign import ccall unsafe "mset_begin"
    cx_mset_begin :: Ptr CMSet -> IO (Ptr CMSetIterator)

foreign import ccall unsafe "mset_end"
    cx_mset_end :: Ptr CMSet -> IO (Ptr CMSetIterator)

foreign import ccall unsafe "mset_back"
    cx_mset_back :: Ptr CMSet -> IO (Ptr CMSetIterator)

foreign import ccall unsafe "mset_index"
    cx_mset_index :: Ptr CMSet -> Word32 -> IO (Ptr CMSetIterator)

foreign import ccall unsafe "mset_get_description"
    cx_mset_get_description :: Ptr CMSet -> IO (CString)

-- Query
-- ---------------------------------------------------------

data CQuery
type QueryPtr = ForeignPtr CQuery
type Op = CInt

instance Manageable CQuery where
    manage = newForeignPtr cx_query_delete

data CQueryVector
data CQueryIterator -- | FIXME: do we need to finalize CQueryIterators?

instance Manageable CQueryVector where
    manage = newForeignPtr cx_vector_delete

instance Manageable CQueryIterator where
    manage = newForeignPtr cx_vector_iterator_delete

foreign import ccall safe "vector_new"
    cx_vector_new :: IO (Ptr CQueryVector)

foreign import ccall safe "&vector_delete"
    cx_vector_delete :: FunPtr (Ptr CQueryVector -> IO ())

foreign import ccall safe "&vector_iterator_delete"
    cx_vector_iterator_delete :: FunPtr (Ptr CQueryIterator -> IO ())

foreign import ccall safe "vector_begin"
    cx_vector_begin :: Ptr CQueryVector -> IO (Ptr CQueryIterator)

foreign import ccall safe "vector_end"
    cx_vector_end :: Ptr CQueryVector -> IO (Ptr CQueryIterator)

foreign import ccall safe "vector_append"
    cx_vector_append :: Ptr CQueryVector -> Ptr CQuery -> IO ()


foreign import ccall unsafe "OP_AND" cx_query_OP_AND :: CInt
foreign import ccall unsafe "OP_OR" cx_query_OP_OR :: CInt
foreign import ccall unsafe "OP_AND_NOT" cx_query_OP_AND_NOT :: CInt
foreign import ccall unsafe "OP_XOR" cx_query_OP_XOR :: CInt
foreign import ccall unsafe "OP_AND_MAYBE" cx_query_OP_AND_MAYBE :: CInt
foreign import ccall unsafe "OP_FILTER" cx_query_OP_FILTER :: CInt
foreign import ccall unsafe "OP_NEAR" cx_query_OP_NEAR :: CInt
foreign import ccall unsafe "OP_PHRASE" cx_query_OP_PHRASE :: CInt
foreign import ccall unsafe "OP_VALUE_RANGE" cx_query_OP_VALUE_RANGE :: CInt
foreign import ccall unsafe "OP_SCALE_WEIGHT" cx_query_OP_SCALE_WEIGHT :: CInt
foreign import ccall unsafe "OP_ELITE_SET" cx_query_OP_ELITE_SET :: CInt
foreign import ccall unsafe "OP_VALUE_GE" cx_query_OP_VALUE_GE :: CInt
foreign import ccall unsafe "OP_VALUE_LE" cx_query_OP_VALUE_LE :: CInt
foreign import ccall unsafe "OP_SYNONYM" cx_query_OP_SYNONYM :: CInt

foreign import ccall unsafe "query_new"
    cx_query_new :: IO (Ptr CQuery)

foreign import ccall unsafe "query_copy"
    cx_query_copy :: Ptr CQuery -> IO (Ptr CQuery)
  
foreign import ccall unsafe "&query_delete"
    cx_query_delete :: FunPtr (Ptr CQuery -> IO ())

foreign import ccall unsafe "query_new_0"
    cx_query_new_0 :: Ptr StdString -> Word32 -> Word32 -> IO (Ptr CQuery)

foreign import ccall unsafe "query_new_1"
    cx_query_new_1 :: Op -> Ptr CQuery -> Ptr CQuery -> IO (Ptr CQuery)

foreign import ccall unsafe "query_new_2"
    cx_query_new_2 :: Op -> Ptr StdString -> Ptr StdString -> IO (Ptr CQuery)

foreign import ccall unsafe "query_new_3"
    cx_query_new_3 :: Op -> Ptr CQueryIterator -> Ptr CQueryIterator -> Word32 -> IO (Ptr CQuery)

foreign import ccall unsafe "query_new_4"
    cx_query_new_4 :: Op -> Ptr CQuery -> Double -> IO (Ptr CQuery)

foreign import ccall unsafe "query_new_5"
    cx_query_new_5 :: Op -> Word32 -> Ptr StdString -> Ptr StdString -> IO (Ptr CQuery)

foreign import ccall unsafe "query_new_6"
    cx_query_new_6 :: Op -> Word32 -> Ptr StdString -> IO (Ptr CQuery)

-- foreign import ccall unsafe "query_new_7"

foreign import ccall unsafe "query_match_all"
    cx_query_match_all :: IO (Ptr CQuery)

foreign import ccall unsafe "query_match_nothing"
    cx_query_match_nothing :: IO (Ptr CQuery)

foreign import ccall unsafe "query_get_length"
    cx_query_get_length :: Ptr CQuery -> IO Word32

foreign import ccall unsafe "query_empty"
    cx_query_empty :: Ptr CQuery -> IO CBool
 
foreign import ccall unsafe "query_serialise"
    cx_query_serialise :: Ptr CQuery -> IO (Ptr StdString)

foreign import ccall unsafe "query_get_terms_begin"
    cx_query_get_terms_begin :: Ptr CQuery -> IO (Ptr CTermIterator)

foreign import ccall unsafe "query_get_terms_end"
    cx_query_get_terms_end :: Ptr CQuery -> IO (Ptr CTermIterator)

foreign import ccall unsafe "query_get_description"
    cx_query_get_description :: Ptr CQuery -> IO (Ptr StdString)

-- QueryParser
-- ---------------------------------------------------------

data CQueryParser
type QueryParserPtr = ForeignPtr CQueryParser

instance Manageable CQueryParser where
    manage = newForeignPtr cx_queryparser_delete

foreign import ccall safe "queryparser_STEM_NONE"
    cx_queryparser_STEM_NONE :: CInt

foreign import ccall safe "queryparser_STEM_SOME"
    cx_queryparser_STEM_SOME :: CInt

foreign import ccall safe "queryparser_STEM_ALL"
    cx_queryparser_STEM_ALL  :: CInt

foreign import ccall unsafe "queryparser_new"
    cx_queryparser_new :: IO (Ptr CQueryParser)

foreign import ccall unsafe "queryparser_copy"
    cx_queryparser_copy :: Ptr CQueryParser -> IO (Ptr CQueryParser)

foreign import ccall unsafe "&queryparser_delete"
    cx_queryparser_delete :: FunPtr (Ptr CQueryParser -> IO ())

foreign import ccall unsafe "queryparser_set_stemmer"
    cx_queryparser_set_stemmer :: Ptr CQueryParser -> Ptr CStem -> IO ()

foreign import ccall unsafe "queryparser_set_stemming_strategy"
    cx_queryparser_set_stemming_strategy :: Ptr CQueryParser -> CInt -> IO ()

foreign import ccall unsafe "queryparser_set_stopper"
    cx_queryparser_set_stopper :: Ptr CQueryParser -> Ptr CStopper -> IO ()

foreign import ccall unsafe "queryparser_set_database"
    cx_queryparser_set_database :: Ptr CQueryParser -> Ptr CDatabase -> IO ()

foreign import ccall unsafe "queryparser_parse_query_simple"
    cx_queryparser_parse_query_simple
        :: Ptr CQueryParser -> Ptr StdString -> IO (Ptr CQuery)

foreign import ccall unsafe "queryparser_add_prefix"
    cx_queryparser_add_prefix
        :: Ptr CQueryParser
        -> Ptr StdString -- ^ field
        -> Ptr StdString -- ^ prefix
        -> IO ()

foreign import ccall unsafe "queryparser_add_boolean_prefix"
    cx_queryparser_add_boolean_prefix
        :: Ptr CQueryParser
        -> Ptr StdString -- ^ field
        -> Ptr StdString -- ^ field
        -> CBool -- ^ exclusive
        -> IO ()

foreign import ccall unsafe "queryparser_get_corrected_query_string"
    cx_queryparser_get_corrected_query_string :: Ptr CQueryParser -> IO (Ptr StdString)

foreign import ccall unsafe "queryparser_get_description"
    cx_queryparser_get_description :: Ptr CQueryParser -> IO (Ptr StdString)

-- Stem
-- ---------------------------------------------------------

data CStem
type StemPtr = ForeignPtr CStem

instance Manageable CStem where
    manage = newForeignPtr cx_stem_delete

foreign import ccall unsafe "stem_copy"
    cx_stem_copy :: Ptr CStem -> IO (Ptr CStem)

foreign import ccall unsafe "stem_new_with_language"
    cx_stem_new_with_language :: CString -> IO (Ptr CStem)

foreign import ccall unsafe "&stem_delete"
    cx_stem_delete :: FunPtr (Ptr CStem -> IO ())

foreign import ccall unsafe "stem_word"
    cx_stem_word :: Ptr CStem -> CString -> IO CString

foreign import ccall unsafe "stem_get_description"
    cx_stem_get_description :: Ptr CStem -> IO CString

foreign import ccall unsafe "stem_get_available_languages"
    cx_stem_get_available_languages :: IO CString

-- Stopper
-- ---------------------------------------------------------

data CStopper

foreign import ccall unsafe "stopper_simple_stopper_new"
    cx_stopper_simple_stopper_new :: IO (Ptr CStopper)

foreign import ccall unsafe "&stopper_delete"
    cx_stopper_delete :: FunPtr (Ptr CStopper -> IO ())

foreign import ccall unsafe "stopper_check"
    cx_stopper_check :: Ptr CStopper -> CString -> IO CBool

foreign import ccall unsafe "stopper_get_description"
    cx_stopper_get_description :: Ptr CStopper -> IO CString

-- | make sure you call this on a Xapian::SimpleStopper
foreign import ccall unsafe "stopper_simple_stopper_add"
    cx_stopper_simple_stopper_add :: Ptr CStopper -> CString -> IO ()



-- MSetIterator
-- ---------------------------------------------------------

data CMSetIterator

instance Manageable CMSetIterator where
    manage = newForeignPtr cx_msetiterator_delete

foreign import ccall unsafe "msetiterator_new"
    cx_msetiterator_new :: IO (Ptr CMSetIterator)

foreign import ccall unsafe "msetiterator_copy"
    cx_msetiterator_copy :: Ptr CMSetIterator -> IO (Ptr CMSetIterator)

foreign import ccall unsafe "&msetiterator_delete"
    cx_msetiterator_delete :: FunPtr (Ptr CMSetIterator -> IO ())

foreign import ccall unsafe "msetiterator_next"
    cx_msetiterator_next :: Ptr CMSetIterator -> IO ()

foreign import ccall unsafe "msetiterator_prev"
    cx_msetiterator_prev :: Ptr CMSetIterator -> IO ()

foreign import ccall unsafe "msetiterator_is_end"
    cx_msetiterator_is_end :: Ptr CMSetIterator -> Ptr CMSetIterator -> IO CBool

foreign import ccall unsafe "msetiterator_get"
    cx_msetiterator_get :: Ptr CMSetIterator -> IO Word32

foreign import ccall unsafe "msetiterator_get_document"
    cx_msetiterator_get_document :: Ptr CMSetIterator -> IO (Ptr CDocument)

foreign import ccall unsafe "msetiterator_get_rank"
    cx_msetiterator_get_rank :: Ptr CMSetIterator -> IO Word32

foreign import ccall unsafe "msetiterator_get_weight"
    cx_msetiterator_get_weight :: Ptr CMSetIterator -> IO Word32

foreign import ccall unsafe "msetiterator_get_collapse_key"
    cx_msetiterator_get_collapse_key :: Ptr CMSetIterator -> IO CString

foreign import ccall unsafe "msetiterator_get_collapse_count"
    cx_msetiterator_get_collapse_count :: Ptr CMSetIterator -> IO Word32

foreign import ccall unsafe "msetiterator_get_percent"
    cx_msetiterator_get_percent :: Ptr CMSetIterator -> IO Int

foreign import ccall unsafe "msetiterator_get_description"
    cx_msetiterator_get_description :: Ptr CMSetIterator -> IO CString


-- PositionIterator
-- ---------------------------------------------------------

data CPositionIterator
type Pos = Word32

instance Manageable CPositionIterator where
    manage = newForeignPtr cx_positioniterator_delete

foreign import ccall unsafe "positioniterator_new"
    cx_positioniterator_new :: IO (Ptr CPositionIterator)

foreign import ccall unsafe "positioniterator_next"
    cx_positioniterator_next :: Ptr CPositionIterator -> IO ()

foreign import ccall unsafe "positioniterator_get"
    cx_positioniterator_get :: Ptr CPositionIterator -> IO Pos

foreign import ccall unsafe "positioniterator_is_end"
    cx_positioniterator_is_end
        :: Ptr CPositionIterator -- ^ current iterator position
        -> Ptr CPositionIterator -- ^ end
        -> IO CBool

foreign import ccall unsafe "positioniterator_copy"
    cx_positioniterator_copy :: Ptr CPositionIterator
                             -> IO (Ptr CPositionIterator)

foreign import ccall unsafe "&positioniterator_delete"
    cx_positioniterator_delete :: FunPtr (Ptr CPositionIterator -> IO ())

foreign import ccall unsafe "positioniterator_skip_to"
    cx_positioniterator_skip_to :: Ptr CPositionIterator -> Word32 -> IO ()

foreign import ccall unsafe "positioniterator_get_description"
    cx_positioniterator_get_description :: Ptr CPositionIterator
                                        -> IO CString

-- PostingIterator
-- ---------------------------------------------------------

data CPostingIterator

instance Manageable CPostingIterator where
    manage = newForeignPtr cx_postingiterator_delete

foreign import ccall unsafe "postingiterator_new"
    cx_postingiterator_new :: IO (Ptr CPostingIterator)

foreign import ccall unsafe "postingiterator_copy"
    cx_postingiterator_copy :: Ptr CPostingIterator -> IO (Ptr CPostingIterator)

foreign import ccall unsafe "&postingiterator_delete"
    cx_postingiterator_delete :: FunPtr (Ptr CPostingIterator -> IO ())

foreign import ccall unsafe "postingiterator_next"
    cx_postingiterator_next :: Ptr CPostingIterator -> IO ()

foreign import ccall unsafe "postingiterator_get"
    cx_postingiterator_get :: Ptr CPostingIterator -> IO Word32

foreign import ccall unsafe "postingiterator_is_end"
    cx_postingiterator_is_end
        :: Ptr CPostingIterator -> Ptr CPostingIterator -> IO CBool

foreign import ccall unsafe "postingiterator_skip_to"
    cx_postingiterator_skip_to
        :: Ptr CPostingIterator
        -> Word32                 -- ^ document ID
        -> IO ()

foreign import ccall unsafe "postingiterator_get_doclength"
    cx_postingiterator_get_doclength :: Ptr CPostingIterator -> IO Word32

foreign import ccall unsafe "postingiterator_get_wdf"
    cx_postingiterator_get_wdf :: Ptr CPostingIterator -> IO Word32

foreign import ccall unsafe "postingiterator_positionlist_begin"
    cx_postingiterator_positionlist_begin
        :: Ptr CPostingIterator -> IO (Ptr CPositionIterator)

foreign import ccall unsafe "postingiterator_positionlist_end"
    cx_postingiterator_positionlist_end
        :: Ptr CPostingIterator -> IO (Ptr CPositionIterator)

foreign import ccall unsafe "postingiterator_get_description"
    cx_postingiterator_get_description :: Ptr CPostingIterator -> IO CString

-- TermIterator
-- ---------------------------------------------------------

data CTermIterator

instance Manageable CTermIterator where
    manage = newForeignPtr cx_termiterator_delete

foreign import ccall unsafe "termiterator_new"
    cx_termiterator_new :: IO (Ptr CTermIterator)

foreign import ccall unsafe "termiterator_copy"
    cx_termiterator_copy :: Ptr CTermIterator -> IO (Ptr CTermIterator)

foreign import ccall unsafe "&termiterator_delete"
    cx_termiterator_delete :: FunPtr (Ptr CTermIterator -> IO ())

foreign import ccall unsafe "termiterator_next"
    cx_termiterator_next :: Ptr CTermIterator -> IO ()

foreign import ccall unsafe "termiterator_is_end"
    cx_termiterator_is_end :: Ptr CTermIterator -> Ptr CTermIterator
                           -> IO CBool

foreign import ccall unsafe "termiterator_get"
    cx_termiterator_get :: Ptr CTermIterator -> IO (Ptr StdString)

foreign import ccall unsafe "termiterator_skip_to"
    cx_termiterator_skip_to :: Ptr CTermIterator -> Ptr StdString -> IO ()

foreign import ccall unsafe "termiterator_get_wdf"
    cx_termiterator_get_wdf :: Ptr CTermIterator -> IO Word32

foreign import ccall unsafe "termiterator_get_termfreq"
    cx_termiterator_get_termfreq :: Ptr CTermIterator -> IO Word32

foreign import ccall unsafe "termiterator_positionlist_count"
    cx_termiterator_positionlist_count :: Ptr CTermIterator -> IO Word32

foreign import ccall unsafe "termiterator_positionlist_begin"
    cx_termiterator_positionlist_begin :: Ptr CTermIterator
                                       -> IO (Ptr CPositionIterator)

foreign import ccall unsafe "termiterator_positionlist_end"
    cx_termiterator_positionlist_end :: Ptr CTermIterator
                                     -> IO (Ptr CPositionIterator)

-- ValueIterator
-- ---------------------------------------------------------

data CValueIterator

instance Manageable CValueIterator where
    manage = newForeignPtr cx_valueiterator_delete

foreign import ccall unsafe "valueiterator_new"
    cx_valueiterator_new :: IO (Ptr CValueIterator)

foreign import ccall unsafe "valueiterator_copy"
    cx_valueiterator_copy :: Ptr CValueIterator -> IO (Ptr CValueIterator)

foreign import ccall unsafe "&valueiterator_delete"
    cx_valueiterator_delete :: FunPtr (Ptr CValueIterator -> IO ())

foreign import ccall unsafe "valueiterator_get"
    cx_valueiterator_get :: Ptr CValueIterator -> IO CString

foreign import ccall unsafe "valueiterator_next"
    cx_valueiterator_next :: Ptr CValueIterator -> IO ()

foreign import ccall unsafe "valueiterator_is_end"
    cx_valueiterator_is_end
        :: Ptr CValueIterator -- ^ current position
        -> Ptr CValueIterator -- ^ end
        -> IO CBool

foreign import ccall unsafe "valueiterator_get_docid"
    cx_valueiterator_get_docid :: Ptr CValueIterator -> IO Word32

foreign import ccall unsafe "valueiterator_get_valueno"
    cx_valueiterator_get_valueno :: Ptr CValueIterator -> IO Word32

foreign import ccall unsafe "valueiterator_skip_to"
    cx_valueiterator_skip_to :: Ptr CValueIterator
                             -> Word32 -- docid or slot
                             -> IO ()

foreign import ccall unsafe "valueiterator_check"
    cx_valueiterator_check :: Ptr CValueIterator
                           -> Word32              -- docid
                           -> IO CBool

foreign import ccall unsafe "valueiterator_get_description"
    cx_valueiterator_get_description :: Ptr CValueIterator -> IO CString


-- TermGenerator
-- ---------------------------------------------------------

data CTermGenerator

instance Manageable CTermGenerator where
    manage = newForeignPtr cx_termgenerator_delete

foreign import ccall unsafe "termgenerator_new"
    cx_termgenerator_new :: IO (Ptr CTermGenerator)

foreign import ccall unsafe "&termgenerator_delete"
    cx_termgenerator_delete :: FunPtr (Ptr CTermGenerator -> IO ())

foreign import ccall unsafe "termgenerator_set_stemmer"
    cx_termgenerator_set_stemmer :: Ptr CTermGenerator -> Ptr CStem -> IO ()

foreign import ccall unsafe "termgenerator_set_stopper"
    cx_termgenerator_set_stopper :: Ptr CTermGenerator -> Ptr CStopper -> IO ()

foreign import ccall unsafe "termgenerator_set_document"
    cx_termgenerator_set_document :: Ptr CTermGenerator -> Ptr CDocument -> IO ()

foreign import ccall unsafe "termgenerator_get_document"
    cx_termgenerator_get_document :: Ptr CTermGenerator -> IO (Ptr CDocument)

-- | database must be writable
foreign import ccall unsafe "termgenerator_set_database"
    cx_termgenerator_set_database :: Ptr CTermGenerator -> Ptr CDatabase -> IO ()

foreign import ccall unsafe "termgenerator_set_flags"
    cx_termgenerator_set_flags
        :: Ptr CTermGenerator
        -> Int -- ^toggle
        -> Int -- ^mask
        -> Int

foreign import ccall unsafe "termgenerator_index_text"
    cx_termgenerator_index_text
        :: Ptr CTermGenerator
        -> Ptr StdString -- ^text
        -> Word32  -- ^weight
        -> Ptr StdString -- ^prefix
        -> IO ()

foreign import ccall unsafe "termgenerator_index_text_wo_positions"
    cx_termgenerator_index_text_wo_positions
        :: Ptr CTermGenerator
        -> Ptr StdString -- ^text
        -> Word32  -- ^weight
        -> Ptr StdString -- ^prefix
        -> IO ()

foreign import ccall unsafe "termgenerator_increase_termpos"
    cx_termgenerator_increase_termpos
        :: Ptr CTermGenerator
        -> Word32  -- ^delta
        -> IO ()

foreign import ccall unsafe "termgenerator_get_termpos"
    cx_termgenerator_get_termpos :: Ptr CTermGenerator -> IO Word32

foreign import ccall unsafe "termgenerator_set_termpos"
    cx_termgenerator_set_termpos :: Ptr CTermGenerator -> Word32 -> IO ()

foreign import ccall unsafe "termgenerator_get_description"
    cx_termgenerator_get_description :: Ptr CTermGenerator -> IO CString
