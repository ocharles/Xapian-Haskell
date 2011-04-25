{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Search.Xapian.Internal.FFI where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import qualified Data.ByteString as BS


class Manageable a where
    manage :: Ptr a -> IO (ForeignPtr a)


-- Generic Database
-- ---------------------------------------------------------

data CDatabase
type DatabasePtr = ForeignPtr CDatabase

instance Manageable CDatabase where
    manage = newForeignPtr cx_database_delete

-- Read-only databases

foreign import ccall "database_new"
    cx_database_new :: IO (Ptr CDatabase)

foreign import ccall "database_new_from_path"
    cx_database_new_from_path
        :: CString            -- ^ database path
        -> Ptr CString        -- ^ string for error messages to be filled in
        -> IO (Ptr CDatabase)

foreign import ccall "database_copy"
    cx_database_copy :: Ptr CDatabase -> IO (Ptr CDatabase)

foreign import ccall "&database_delete"
    cx_database_delete :: FunPtr(Ptr CDatabase -> IO ())

foreign import ccall "database_add_database"
    cx_database_add_database :: Ptr CDatabase -> Ptr CDatabase -> IO ()

foreign import ccall "database_reopen"
    cx_database_reopen :: Ptr CDatabase -> IO ()

foreign import ccall "database_close"
    cx_database_close :: Ptr CDatabase -> IO ()

foreign import ccall "database_get_description"
    cx_database_get_description :: Ptr CDatabase -> IO CString

foreign import ccall "database_postlist_begin"
    cx_database_postlist_begin :: Ptr CDatabase -> IO (Ptr CPostingIterator)

foreign import ccall "database_postlist_end"
    cx_database_postlist_end :: Ptr CDatabase -> IO (Ptr CPostingIterator)

foreign import ccall "database_termlist_begin"
    cx_database_termlist_begin :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_termlist_end"
    cx_database_termlist_end :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_has_positions"
    cx_database_has_positions :: Ptr CDatabase -> IO Bool

foreign import ccall "database_positionlist_begin"
    cx_database_positionlist_begin :: Ptr CDatabase -> IO (Ptr CPositionIterator)

foreign import ccall "database_positionlist_end"
    cx_database_positionlist_end :: Ptr CDatabase -> IO (Ptr CPositionIterator)

foreign import ccall "database_allterms_begin"
    cx_database_allterms_begin :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_allterms_end"
    cx_database_allterms_end:: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_allterms_with_prefix_begin"
    cx_database_allterms_with_prefix_begin
        :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_allterms_with_prefix_end"
    cx_database_allterms_with_prefix_end
        :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_get_doccount"
    cx_database_get_doccount :: Ptr CDatabase -> IO CUInt

foreign import ccall "database_get_lastdocid"
    cx_database_get_lastdocid :: Ptr CDatabase -> IO CUInt

foreign import ccall "database_get_avlength"
    cx_database_get_avlength :: Ptr CDatabase -> IO Double

foreign import ccall "database_get_termfreq"
    cx_database_get_termfreq :: Ptr CDatabase -> CString -> IO CUInt

foreign import ccall "database_get_value_freq"
    cx_database_get_value_freq :: Ptr CDatabase -> CString -> IO CUInt

foreign import ccall "database_get_value_lower_bound"
    cx_database_get_value_lower_bound :: Ptr CDatabase -> CUInt -> IO CUInt

foreign import ccall "database_get_doclength_lower_bound"
    cx_database_get_doclength_lower_bound :: Ptr CDatabase -> CUInt -> IO CUInt

foreign import ccall "database_get_wdf_upper_bound"
    cx_database_wdf_upper_bound :: Ptr CDatabase -> CString -> IO CUInt

foreign import ccall "database_valuestream_begin"
    cx_database_valuestream_begin
        :: Ptr CDatabase -> CUInt -> IO (Ptr CValueIterator)

foreign import ccall "database_valuestream_end"
    cx_database_valuestream_end
        :: Ptr CDatabase -> CUInt -> IO (Ptr CValueIterator)

foreign import ccall "database_get_doclength"
    cx_database_get_doclength :: Ptr CDatabase -> CUInt -> CUInt

foreign import ccall "database_keep_alive"
    cx_database_keep_alive :: Ptr CDatabase -> IO ()

foreign import ccall "database_get_document"
    cx_database_get_document :: Ptr CDatabase -> CUInt -> IO (Ptr CDocument)

foreign import ccall "database_get_spelling_suggestion"
    cx_database_get_spelling_suggestion
        :: Ptr CDatabase
        -> CString       -- ^ word
        -> CUInt         -- ^ maximum edit distance
        -> IO CString    -- ^ suggested word

foreign import ccall "database_spellings_begin"
    cx_database_spellings_begin :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_spellings_end"
    cx_database_spellings_end :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_synonyms_begin"
    cx_database_synonyms_begin :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_synonyms_end"
    cx_database_synonyms_end :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_synonym_keys_begin"
    cx_database_synonym_keys_begin
        :: Ptr CDatabase
        -> CString                -- ^ prefix
        -> IO (Ptr CTermIterator)

-- | see @cx_database_synonym_keys_begin@
foreign import ccall "database_synonym_keys_end"
    cx_database_synonym_keys_end
        :: Ptr CDatabase
        -> CString
        -> IO (Ptr CTermIterator)

foreign import ccall "database_get_metadata"
    cx_database_get_metadata
        :: Ptr CDatabase
        -> CString       -- ^ key
        -> CString       -- ^ value

foreign import ccall "database_metadata_keys_begin"
    cx_database_metadata_keys_begin :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_metadata_keys_end"
    cx_database_metadata_keys_end :: Ptr CDatabase -> IO (Ptr CTermIterator)

foreign import ccall "database_get_uuid"
    cx_database_get_uuid :: Ptr CDatabase -> IO (CString)

-- Writable database
-- ---------------------------------------------------------

type DbAction = Int
newtype WritableDatabasePtr = WritableDatabasePtr (DatabasePtr)

-- FIXME: should be instance of Writable
manageWritableDatabase = fmap WritableDatabasePtr . manage

foreign import ccall "DB_CREATE_OR_OPEN"
  cx_database_DB_CREATE_OR_OPEN :: DbAction

foreign import ccall "DB_CREATE"
  cx_database_DB_CREATE :: DbAction

foreign import ccall "DB_CREATE_OR_OVERWRITE"
  cx_database_DB_CREATE_OR_OVERWRITE :: DbAction

foreign import ccall "DB_OPEN"
  cx_database_DB_OPEN :: DbAction

-- ensure you only use following functions on writable databases, i.e.
-- databases constructed via database_new_writable

foreign import ccall "database_writable_new"
    cx_database_writable_new :: IO (Ptr CDatabase)

foreign import ccall "database_writable_new_from_path"
    cx_database_writable_new_from_path
        :: CString            -- ^ database path
        -> DbAction           -- ^ how to open the database
        -> Ptr CString        -- ^ string for error messages to be filled in
        -> IO (Ptr CDatabase)

foreign import ccall "database_writable_copy"
    cx_database_writable_copy :: Ptr CDatabase -> IO (Ptr CDatabase)

foreign import ccall "&database_writable_delete"
    cx_database_writable_delete :: FunPtr (Ptr CDatabase -> IO ())

foreign import ccall "database_commit"
    cx_database_commit :: Ptr CDatabase -> IO ()

foreign import ccall "database_begin_transaction"
    cx_database_begin_transaction
        :: Ptr CDatabase
        -> Bool          -- ^ flushed
        -> IO ()

foreign import ccall "database_commit_transaction"
    cx_database_commit_transaction :: Ptr CDatabase -> IO ()

foreign import ccall "database_cancel_transaction"
    cx_database_cancel_transaction :: Ptr CDatabase -> IO ()

foreign import ccall "database_add_document"
    cx_database_add_document :: Ptr CDatabase -> Ptr CDocument -> IO CUInt

foreign import ccall "database_delete_document_by_id"
    cx_database_delete_document_by_id :: Ptr CDatabase -> CUInt -> IO ()

foreign import ccall "database_delete_document_by_term"
    cx_database_delete_document_by_term :: Ptr CDatabase -> CString -> IO ()

foreign import ccall "database_replace_document"
    cx_database_replace_document :: Ptr CDatabase -> CUInt -> IO (Ptr CDocument)

foreign import ccall "database_add_spelling"
    cx_database_add_spelling
        :: Ptr CDatabase
        -> CString       -- ^ word
        -> CUInt         -- ^ frequency increase
        -> IO ()

foreign import ccall "database_remove_spelling"
    cx_database_remove_spelling
        :: Ptr CDatabase
        -> CString       -- ^ word
        -> CUInt         -- ^ frequency decrease
        -> IO ()

foreign import ccall "database_add_synonym"
    cx_database_add_synonym
        :: Ptr CDatabase
        -> CString       -- ^ term
        -> CString       -- ^ synonym
        -> IO ()

foreign import ccall "database_remove_synonym"
    cx_database_remove_synonym
        :: Ptr CDatabase
        -> CString       -- ^ term
        -> CString       -- ^ synonym
        -> IO ()

foreign import ccall "database_clear_synonyms"
    cx_database_clear_synonyms :: Ptr CDatabase -> CString -> IO ()

foreign import ccall "database_set_metadata"
    cx_database_set_metadata
        :: Ptr CDatabase
        -> CString       -- ^ key
        -> CString       -- ^ value
        -> IO ()

foreign import ccall "database_writable_get_description"
    cx_database_writable_get_description :: Ptr CDatabase -> IO CString

-- Document
-- ---------------------------------------------------------

data CDocument
type DocumentPtr = ForeignPtr CDocument

instance Manageable CDocument where
    manage = newForeignPtr cx_document_delete

foreign import ccall "document_new"
    cx_document_new :: IO (Ptr CDocument)

foreign import ccall "document_copy"
    cx_document_copy :: Ptr CDocument -> IO (Ptr CDocument)

foreign import ccall "&document_delete"
    cx_document_delete :: FunPtr (Ptr CDocument -> IO ())

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


-- Enquire
-- ---------------------------------------------------------

data CEnquire
type EnquirePtr = ForeignPtr CEnquire

instance Manageable CEnquire where
    manage = newForeignPtr cx_enquire_delete

foreign import ccall "enquire_new"
    cx_enquire_new :: Ptr CDatabase -> IO (Ptr CEnquire)

foreign import ccall "&enquire_delete"
    cx_enquire_delete :: FunPtr (Ptr CEnquire -> IO ())

foreign import ccall "enquire_set_query"
    cx_enquire_set_query
        :: Ptr CEnquire
        -> Ptr CQuery
        -> CUInt          -- ^ query length
        -> IO ()

foreign import ccall "enquire_get_mset"
    cx_enquire_get_mset :: Ptr CEnquire
                        -> CUInt        -- ^ skip n items
                        -> CUInt        -- ^ maximum amount of items
                        -> IO (Ptr CMSet)

-- MSet
-- ---------------------------------------------------------

data CMSet

foreign import ccall "mset_new"
    cx_mset_new :: IO (Ptr CMSet)

foreign import ccall "mset_copy"
    cx_mset_copy :: Ptr CMSet -> IO (Ptr CMSet)

foreign import ccall "&mset_delete"
    cx_mset_delete :: FunPtr (Ptr CMSet -> IO ())

foreign import ccall "mset_fetch_all"
    cx_mset_fetch_all :: Ptr CMSet -> IO ()

foreign import ccall "mset_fetch_one"
    cx_mset_fetch_one :: Ptr CMSet -> Ptr CMSetIterator -> IO ()

foreign import ccall "mset_fetch_many"
    cx_mset_fetch_many
        :: Ptr CMSet
        -> Ptr CMSetIterator   -- ^ begin
        -> Ptr CMSetIterator   -- ^ end

foreign import ccall "mset_convert_weight_to_percent"
    cx_mset_convert_weight_to_percent :: Ptr CMSet -> Double -> IO Int

foreign import ccall "mset_convert_document_to_percent"
    cx_mset_convert_document_to_percent
        :: Ptr CMSet -> Ptr CMSetIterator -> IO Int

foreign import ccall "mset_get_termfreq"
    cx_mset_get_termfreq :: Ptr CMSet -> CString -> IO CUInt

foreign import ccall "mset_get_termweight"
    cx_mset_get_termweight :: Ptr CMSet -> CString -> IO Double

foreign import ccall "mset_get_firstitem"
    cx_mset_get_firstitem :: Ptr CMSet -> IO CUInt

foreign import ccall "mset_get_matches_lower_bound"
    cx_mset_get_matches_lower_bound :: Ptr CMSet -> IO CUInt

foreign import ccall "mset_get_matches_estimated"
    cx_mset_get_matches_estimated :: Ptr CMSet -> IO CUInt

foreign import ccall "mset_get_matches_upper_bound"
    cx_mset_get_matches_upper_bound :: Ptr CMSet -> IO CUInt

foreign import ccall "mset_get_uncollapsed_matches_lower_bound"
    cx_mset_get_uncollapsed_matches_lower_bound :: Ptr CMSet -> IO CUInt  

foreign import ccall "mset_get_uncollapsed_matches_estimated"
    cx_mset_get_uncollapsed_matches_estimated :: Ptr CMSet -> IO CUInt

foreign import ccall "mset_get_uncollapsed_matches_upper_bound"
    cx_mset_get_uncollapsed_matches_upper_bound :: Ptr CMSet -> IO CUInt

foreign import ccall "mset_get_max_possible"
    cx_mset_get_max_possible :: Ptr CMSet -> IO Double

foreign import ccall "mset_get_max_attained"
    cx_mset_get_max_attained :: Ptr CMSet -> IO Double

foreign import ccall "mset_size"
    cx_mset_size :: Ptr CMSet -> IO CUInt

foreign import ccall "mset_max_size"
    cx_mset_max_size :: Ptr CMSet -> IO CUInt

foreign import ccall "mset_empty"
    cx_mset_empty :: Ptr CMSet -> IO Bool

foreign import ccall "mset_swap"
    cx_mset_swap :: Ptr CMSet -> Ptr CMSet -> IO ()

foreign import ccall "mset_begin"
    cx_mset_begin :: Ptr CMSet -> IO (Ptr CMSetIterator)

foreign import ccall "mset_end"
    cx_mset_end :: Ptr CMSet -> IO (Ptr CMSetIterator)

foreign import ccall "mset_back"
    cx_mset_back :: Ptr CMSet -> IO (Ptr CMSetIterator)

foreign import ccall "mset_index"
    cx_mset_index :: Ptr CMSet -> CUInt -> IO (Ptr CMSetIterator)

foreign import ccall "mset_get_description"
    cx_mset_get_description :: Ptr CMSet -> IO (CString)

-- Query
-- ---------------------------------------------------------

data CQuery
type QueryPtr = ForeignPtr CQuery
type Op = Int

instance Manageable CQuery where
    manage = newForeignPtr cx_query_delete

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
  
foreign import ccall "&query_delete"
    cx_query_delete :: FunPtr (Ptr CQuery -> IO ())

foreign import ccall "query_new_0"
    cx_query_new_0 :: CString -> CUInt -> CUInt -> IO (Ptr CQuery)

foreign import ccall "query_new_1"
    cx_query_new_1 :: Op -> Ptr CQuery -> Ptr CQuery -> IO (Ptr CQuery)

foreign import ccall "query_new_2"
    cx_query_new_2 :: Op -> CString -> CString -> IO (Ptr CQuery)

--foreign import ccall "query_new_3"
--    cx_query_new_3 :: Op -> CString -> CString -> IO (Ptr CQuery)

foreign import ccall "query_new_4"
    cx_query_new_4 :: Op -> Ptr CQuery -> Double -> IO (Ptr CQuery)

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


-- Stem
-- ---------------------------------------------------------

data CStem
type StemPtr = ForeignPtr CStem

instance Manageable CStem where
    manage = newForeignPtr cx_stem_delete

foreign import ccall "stem_copy"
    cx_stem_copy :: Ptr CStem -> IO (Ptr CStem)

foreign import ccall "stem_new_with_language"
    cx_stem_new_with_language :: CString -> IO (Ptr CStem)

foreign import ccall "&stem_delete"
    cx_stem_delete :: FunPtr (Ptr CStem -> IO ())

foreign import ccall "stem_word"
    cx_stem_word :: Ptr CStem -> CString -> IO CString

foreign import ccall "stem_get_description"
    cx_stem_get_description :: Ptr CStem -> IO CString

foreign import ccall "stem_get_available_languages"
    cx_stem_get_available_languages :: IO CString

-- Stopper
-- ---------------------------------------------------------

data CStopper

-- MSetIterator
-- ---------------------------------------------------------

data CMSetIterator

instance Manageable CMSetIterator where
    manage = newForeignPtr cx_msetiterator_delete

foreign import ccall "msetiterator_new"
    cx_msetiterator_new :: IO (Ptr CMSetIterator)

foreign import ccall "msetiterator_copy"
    cx_msetiterator_copy :: Ptr CMSetIterator -> IO (Ptr CMSetIterator)

foreign import ccall "&msetiterator_delete"
    cx_msetiterator_delete :: FunPtr (Ptr CMSetIterator -> IO ())

foreign import ccall "msetiterator_next"
    cx_msetiterator_next :: Ptr CMSetIterator -> IO ()

foreign import ccall "msetiterator_prev"
    cx_msetiterator_prev :: Ptr CMSetIterator -> IO ()

foreign import ccall "msetiterator_is_end"
    cx_msetiterator_is_end :: Ptr CMSetIterator -> Ptr CMSetIterator -> IO Bool

foreign import ccall "msetiterator_get"
    cx_msetiterator_get :: Ptr CMSetIterator -> IO Word32

foreign import ccall "msetiterator_get_document"
    cx_msetiterator_get_document :: Ptr CMSetIterator -> IO (Ptr CDocument)

foreign import ccall "msetiterator_get_rank"
    cx_msetiterator_get_rank :: Ptr CMSetIterator -> IO CUInt

foreign import ccall "msetiterator_get_weight"
    cx_msetiterator_get_weight :: Ptr CMSetIterator -> IO CUInt

foreign import ccall "msetiterator_get_collapse_key"
    cx_msetiterator_get_collapse_key :: Ptr CMSetIterator -> IO CString

foreign import ccall "msetiterator_get_collapse_count"
    cx_msetiterator_get_collapse_count :: Ptr CMSetIterator -> IO CUInt

foreign import ccall "msetiterator_get_percent"
    cx_msetiterator_get_percent :: Ptr CMSetIterator -> IO Int

foreign import ccall "msetiterator_get_description"
    cx_msetiterator_get_description :: Ptr CMSetIterator -> IO CString


-- PositionIterator
-- ---------------------------------------------------------

data CPositionIterator

instance Manageable CPositionIterator where
    manage = newForeignPtr cx_positioniterator_delete

foreign import ccall "positioniterator_new"
    cx_positioniterator_new :: IO (Ptr CPositionIterator)

foreign import ccall "positioniterator_copy"
    cx_positioniterator_copy :: Ptr CPositionIterator
                             -> IO (Ptr CPositionIterator)

foreign import ccall "&positioniterator_delete"
    cx_positioniterator_delete :: FunPtr (Ptr CPositionIterator -> IO ())

foreign import ccall "positioniterator_skip_to"
    cx_positioniterator_skip_to :: Ptr CPositionIterator -> CUInt -> IO ()

foreign import ccall "positioniterator_get_description"
    cx_positioniterator_get_description :: Ptr CPositionIterator
                                        -> IO CString

-- PostingIterator
-- ---------------------------------------------------------

data CPostingIterator

instance Manageable CPostingIterator where
    manage = newForeignPtr cx_postingiterator_delete

foreign import ccall "postingiterator_new"
    cx_postingiterator_new :: IO (Ptr CPostingIterator)

foreign import ccall "postingiterator_copy"
    cx_postingiterator_copy :: Ptr CPostingIterator -> IO (Ptr CPostingIterator)

foreign import ccall "&postingiterator_delete"
    cx_postingiterator_delete :: FunPtr (Ptr CPostingIterator -> IO ())

foreign import ccall "postingiterator_get"
    cx_postingiterator_get :: Ptr CPostingIterator -> IO CUInt

foreign import ccall "postingiterator_skip_to"
    cx_postingiterator_skip_to
        :: Ptr CPostingIterator
        -> CUInt                 -- ^ document ID
        -> IO ()

foreign import ccall "postingiterator_get_doclength"
    cx_postingiterator_get_doclength :: Ptr CPostingIterator -> IO CUInt

foreign import ccall "postingiterator_get_wdf"
    cx_postingiterator_get_wdf :: Ptr CPostingIterator -> IO CUInt

foreign import ccall "postingiterator_positionlist_begin"
    cx_postingiterator_positionlist_begin
        :: Ptr CPostingIterator -> IO (Ptr CPositionIterator)

foreign import ccall "postingiterator_positionlist_end"
    cx_postingiterator_positionlist_end
        :: Ptr CPostingIterator -> IO (Ptr CPositionIterator)

foreign import ccall "postingiterator_get_description"
    cx_postingiterator_get_description :: Ptr CPostingIterator -> IO CString

-- TermIterator
-- ---------------------------------------------------------

data CTermIterator

instance Manageable CTermIterator where
    manage = newForeignPtr cx_termiterator_delete

foreign import ccall "termiterator_new"
    cx_termiterator_new :: IO (Ptr CTermIterator)

foreign import ccall "termiterator_copy"
    cx_termiterator_copy :: Ptr CTermIterator -> IO (Ptr CTermIterator)

foreign import ccall "&termiterator_delete"
    cx_termiterator_delete :: FunPtr (Ptr CTermIterator -> IO ())

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

instance Manageable CValueIterator where
    manage = newForeignPtr cx_valueiterator_delete

foreign import ccall "valueiterator_new"
    cx_valueiterator_new :: IO (Ptr CValueIterator)

foreign import ccall "valueiterator_copy"
    cx_valueiterator_copy :: Ptr CValueIterator -> IO (Ptr CValueIterator)

foreign import ccall "&valueiterator_delete"
    cx_valueiterator_delete :: FunPtr (Ptr CValueIterator -> IO ())

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
