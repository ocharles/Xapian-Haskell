module Search.Xapian.Database
      ( openReadOnly
      , openReadWrite
      , replaceDocument
      ) where

import Prelude hiding (words)
import Foreign
import Foreign.C.String
import Control.Monad (forM)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (ByteString, pack, useAsCString)
import Data.Either (rights)
import Data.Enumerator (Iteratee (..))

import Search.Xapian.Types
import Search.Xapian.Internal.Types
import Search.Xapian.Internal.Utils
import Search.Xapian.Internal.FFI
import qualified Search.Xapian.Query as Q


-- * interfacing the database

instance ReadableDatabase ReadOnlyDB where
    search db@(ReadOnlyDB dbmptr) query (QueryRange off lim) =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
         do querymptr <- Q.compileQuery db query
            enquiremptr <- manage =<< cx_enquire_new dbptr
            withForeignPtr querymptr $ \queryptr ->
                withForeignPtr enquiremptr $ \enquire ->
                 do cx_enquire_set_query enquire queryptr 0
                    msetmptr <- manage =<< cx_enquire_get_mset enquire
                        (fromIntegral off) (fromIntegral lim)
                    withForeignPtr msetmptr $ \mset ->
                     do begin <- manage =<< cx_mset_begin mset
                        end   <- manage =<< cx_mset_end   mset
                        docidlist <- collectDocIds begin end
                        doclist <- runXapian $ fmap rights $ 
                         do forM docidlist $ getDocument db
                        return (MSet msetmptr, doclist)

    getDocument (ReadOnlyDB dbmptr) docid@(DocId id') =
        liftIO $
         withForeignPtr dbmptr $ \dbptr ->
         handleError dbptr $ \docmptr ->
             return . Right $ Document docmptr docid
      where
        handleError dbptr action =
          alloca $ \errorPtr ->
           do handle <- cx_database_get_document dbptr (fromIntegral id') errorPtr
              if handle == nullPtr
                 then do err <- peekCString =<< peek errorPtr
                         return . Left $ Error (Just DocNotFoundError) err
                 else manage handle >>= action

    getMetadata (ReadOnlyDB dbmptr) key =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        useAsCCString key $ \cckey ->
        fromCCString =<< manage =<< cx_database_get_metadata dbptr cckey

    getMetadataKeys (ReadOnlyDB dbmptr) prefix chunksize step =
        Iteratee $
         do (b,e) <- liftIO $ withForeignPtr dbmptr $ \dbptr ->
             do useAsCCString prefix $ \ccprefix ->
                 do b <- manage =<< cx_database_metadata_keys_begin dbptr ccprefix
                    e <- manage =<< cx_database_metadata_keys_end   dbptr ccprefix
                    return (b,e)
            runIteratee $ enumerateTerms' b e chunksize step

    getSynonyms (ReadOnlyDB dbmptr) term chunksize step =
        Iteratee $
         do (b,e) <- liftIO $ withForeignPtr dbmptr $ \dbptr ->
             do useAsCCString term $ \ccterm ->
                 do b <- manage =<< cx_database_synonyms_begin dbptr ccterm
                    e <- manage =<< cx_database_synonyms_end   dbptr ccterm
                    return (b,e)
            runIteratee $ enumerateTerms' b e chunksize step

    getSynonymKeys (ReadOnlyDB dbmptr) prefix chunksize step =
        Iteratee $
         do (b,e) <- liftIO $ withForeignPtr dbmptr $ \dbptr ->
             do useAsCCString prefix $ \ccprefix ->
                 do b <- manage =<< cx_database_synonym_keys_begin dbptr ccprefix
                    e <- manage =<< cx_database_synonym_keys_end   dbptr ccprefix
                    return (b,e)
            runIteratee $ enumerateTerms' b e chunksize step

    getSpellings (ReadOnlyDB dbmptr) chunksize step =
        Iteratee $
         do (b,e) <- liftIO $ withForeignPtr dbmptr $ \dbptr ->
             do b <- manage =<< cx_database_spellings_begin dbptr
                e <- manage =<< cx_database_spellings_end   dbptr
                return (b,e)
            runIteratee $ enumerateTermsWdf b e chunksize step
    
    suggestSpelling (ReadOnlyDB dbmptr) term max_edit_distance =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        useAsCCString term $ \ccterm ->
         do cx_database_get_spelling_suggestion
                dbptr
                ccterm
                (fromIntegral max_edit_distance)
                >>= manage >>= fromCCString

    getPostings (ReadOnlyDB dbmptr) term chunksize step =
        Iteratee $
         do (b,e) <- liftIO $ withForeignPtr dbmptr $ \dbptr ->
             do useAsCCString term $ \ccterm ->
                 do b <- manage =<< cx_database_postlist_begin dbptr ccterm
                    e <- manage =<< cx_database_postlist_end   dbptr ccterm
                    return (b,e)
            runIteratee $ enumeratePostings b e chunksize step

    getDocCount (ReadOnlyDB dbmptr) =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        fmap fromIntegral $ cx_database_get_doccount dbptr

    getAllTerms (ReadOnlyDB dbmptr) chunksize step =
        Iteratee $
         do (b,e) <- liftIO $ withForeignPtr dbmptr $ \dbptr ->
             do b <- manage =<< cx_database_allterms_begin dbptr
                e <- manage =<< cx_database_allterms_end   dbptr
                return (b,e)
            runIteratee $ enumerateTerms' b e chunksize step

    termExists = __withDbAndTerm $ \dbptr ccterm ->
        fmap (==1) $ cx_database_term_exists dbptr ccterm

    termFreq = __withDbAndTerm $ \dbptr ccterm ->
        fmap fromIntegral $ cx_database_get_termfreq dbptr ccterm

    collFreq = __withDbAndTerm $ \dbptr ccterm ->
        fmap fromIntegral $ cx_database_get_collection_freq dbptr ccterm

    wdfMax   = __withDbAndTerm $ \dbptr ccterm ->
        fmap fromIntegral $ cx_database_get_wdf_upper_bound dbptr ccterm

    getUUID (ReadOnlyDB dbmptr) =
        liftIO $ withForeignPtr dbmptr $ \dbptr ->
        cx_database_get_uuid dbptr >>= manage >>= fromCCString

__withDbAndTerm
    :: (Ptr CDatabase -> Ptr StdString -> IO a)
    -> (ReadOnlyDB    -> ByteString    -> XapianM a)
__withDbAndTerm f (ReadOnlyDB dbmptr) term =
    liftIO $
    withForeignPtr dbmptr $ \dbptr ->
    useAsCCString term $ f dbptr

instance ReadableDatabase ReadWriteDB where
  search          = __withCastedDB search
  getDocument     = __withCastedDB getDocument
  getMetadata     = __withCastedDB getMetadata
  getMetadataKeys = __withCastedDB getMetadataKeys
  getSynonyms     = __withCastedDB getSynonyms
  getSynonymKeys  = __withCastedDB getSynonymKeys
  getSpellings    = __withCastedDB getSpellings
  suggestSpelling = __withCastedDB suggestSpelling
  getPostings     = __withCastedDB getPostings
  getDocCount     = __withCastedDB getDocCount
  getAllTerms     = __withCastedDB getAllTerms

  termExists      = __withCastedDB termExists
  termFreq        = __withCastedDB termFreq
  collFreq        = __withCastedDB collFreq
  wdfMax          = __withCastedDB wdfMax
  getUUID         = __withCastedDB getUUID

__withCastedDB
    :: (ReadOnlyDB  -> r)
    -> (ReadWriteDB -> r)
__withCastedDB f (ReadWriteDB db) = f (ReadOnlyDB $ castForeignPtr db)

-- * opening databases

-- | @openDatabase filename@ will open the database at @filename@ in readonly
-- mode. If the database could not be opened, a string error message will be
-- returned in the left value.
--
openReadOnly :: FilePath -> IO (Either Error ReadOnlyDB)
openReadOnly path =
  useAsCString (pack path) $ \cpath ->
  alloca $ \errorPtr ->
   do dbHandle <- cx_database_new_from_path cpath errorPtr
      if dbHandle == nullPtr
         then do err <- peekCString =<< peek errorPtr
                 return (Left $ Error (Just DatabaseOpeningError) err)
         else do fmap (Right . ReadOnlyDB) (manage dbHandle)

-- FIXME 'mode' is a bad term here...
-- | @openWritableDatabase mode filename@ will open the database at @filename@
-- with the mode specified by @mode@. If the database could not be opened
-- successfully, a string error message will be stored in the left value.
--
openReadWrite :: InitDBOption -> FilePath -> IO (Either Error ReadWriteDB)
openReadWrite option path =
  useAsCString (pack path) $ \cpath ->
  alloca $ \errorPtr ->
   do let option' = packInitDBOption option
      dbHandle <- cx_database_writable_new_from_path cpath option' errorPtr
      if dbHandle == nullPtr
         then do err <- peekCString =<< peek errorPtr
                 return (Left $ Error (Just DatabaseOpeningError) err)
         else do fmap (Right . ReadWriteDB) (manage dbHandle)


instance WritableDatabase ReadWriteDB where

    addDocument (ReadWriteDB dbmptr) doc =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        withForeignPtr (docPtr doc) $ \docptr ->
             do fmap DocId $ cx_database_add_document dbptr docptr

    delDocumentById (ReadWriteDB dbmptr) (DocId id') =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        cx_database_delete_document_by_id dbptr (fromIntegral id')

    delDocumentByTerm (ReadWriteDB dbmptr) term =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        useAsCCString term $
        cx_database_delete_document_by_term dbptr

    setMetadata (ReadWriteDB dbmptr) key val =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        useAsCCString key $ \cckey ->
        useAsCCString val $ \ccval ->
        cx_database_set_metadata dbptr cckey ccval

    addSynonym (ReadWriteDB dbmptr) term synonym =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        useAsCCString term $ \ccterm ->
        useAsCCString synonym $ \ccsyno ->
        cx_database_add_synonym dbptr ccterm ccsyno

    delSynonym (ReadWriteDB dbmptr) term synonym =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        useAsCCString term $ \ccterm ->
        useAsCCString synonym $ \ccsyno ->
        cx_database_remove_synonym dbptr ccterm ccsyno

    clearSynonyms (ReadWriteDB dbmptr) prefix =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        useAsCCString prefix $ \ccprefix ->
        cx_database_clear_synonyms dbptr ccprefix

    addSpelling (ReadWriteDB dbmptr) term freqinc =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        useAsCCString term $ \ccterm ->
        cx_database_add_spelling dbptr ccterm (fromIntegral freqinc)

    delSpelling (ReadWriteDB dbmptr) term freqdec =
        liftIO $
        withForeignPtr dbmptr $ \dbptr ->
        useAsCCString term $ \ccterm ->
        cx_database_remove_spelling dbptr ccterm (fromIntegral freqdec)


replaceDocument :: ReadWriteDB -> DocumentId -> Document -> XapianM ()
replaceDocument (ReadWriteDB dbmptr) (DocId id') doc =
    liftIO $
    withForeignPtr dbmptr $ \dbptr ->
    withForeignPtr (docPtr doc) $ \docptr ->
    cx_database_replace_document dbptr (fromIntegral id') docptr
