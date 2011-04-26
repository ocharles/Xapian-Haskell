module Search.Xapian.Database where

import Prelude hiding (words)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad (forM_)
import Control.Applicative
import Control.Arrow ((***))
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, useAsCString, words)
import Data.Either (rights)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Serialize
import Data.Traversable (sequenceA)

import Search.Xapian.Document
import Search.Xapian.Types
import Search.Xapian.Internal.Types
import Search.Xapian.Internal.Utils
import Search.Xapian.Internal.FFI
import qualified Search.Xapian.Query as Q


-- * interfacing the database

instance ReadableDatabase Database where
  search database@(Database dbFPtr) query (QueryRange off lim) =
   do queryFPtr <- Q.compileQuery query
      withForeignPtr dbFPtr $ \dbPtr ->
          withForeignPtr queryFPtr $ \queryPtr ->
           do enquire <- cx_enquire_new dbPtr
              cx_enquire_set_query enquire queryPtr 0
              mset <- cx_enquire_get_mset
                          enquire (fromIntegral off) (fromIntegral lim)
              begin <- manage =<< cx_mset_begin mset
              end   <- manage =<< cx_mset_end   mset
              -- FIXME: should the user be notified if some documents cannot be
                                                       -- opened?
              (MSet . rights) <$>
                  (mapM (getDocument database) =<< collectDocIds begin end)

  getDocument (Database database) docId@(DocId id') =
       withForeignPtr database $ \dbPtr ->
       handleError dbPtr $ \docPtr ->
        do docFPtr <- newForeignPtr cx_document_delete docPtr
           eitherDocDat  <- getDocumentData docFPtr
           case eitherDocDat of
                Left err  -> return (Left err)
                Right dat ->
                 do terms <- getDocumentTerms docFPtr
                    values <- getDocumentValues docFPtr
                    return . Right $
                        (document dat){ documentLazyTerms = terms
                                      , documentLazyValues = values
                                      , documentLazyFields = fieldsFromTerms terms
                                      , documentId = Just docId }
    where
      handleError dbPtr action =
        alloca $ \errorPtr ->
         do handle <- cx_database_get_document dbPtr (fromIntegral id') errorPtr
            if handle == nullPtr
               then do err <- peekCString =<< peek errorPtr
                       return . Left $ Error (Just DocNotFoundError) err
               else do action handle

instance ReadableDatabase WritableDatabase where
  search (WritableDatabase db) = search db
  getDocument (WritableDatabase db) id' = getDocument db id'

-- * opening databases

-- | @openDatabase filename@ will open the database at @filename@ in readonly
-- mode. If the database could not be opened, a string error message will be
-- returned in the left value.
--
openDatabase :: (Serialize dat, Prefixable fields)
             => FilePath
             -> IO (Either Error (Database fields dat))
openDatabase path =
  useAsCString (pack path) $ \cpath ->
  alloca $ \errorPtr ->
   do dbHandle <- cx_database_new_from_path cpath errorPtr
      if dbHandle == nullPtr
         then do err <- peekCString =<< peek errorPtr
                 return (Left $ Error (Just DatabaseOpeningError) err)
         else do fmap (Right . Database) (manage dbHandle)

-- FIXME 'mode' is a bad term here...
-- | @openWritableDatabase mode filename@ will open the database at @filename@
-- with the mode specified by @mode@. If the database could not be opened
-- successfully, a string error message will be stored in the left value.
--
openWritableDatabase :: (Serialize dat, Prefixable fields)
                     => InitDBOption
                     -> FilePath
                     -> IO (Either Error (WritableDatabase fields dat))
openWritableDatabase option path =
  useAsCString (pack path) $ \cpath ->
  alloca $ \errorPtr ->
   do let option' = packInitDBOption option
      dbHandle <- cx_database_writable_new_from_path cpath option' errorPtr
      if dbHandle == nullPtr
         then do err <- peekCString =<< peek errorPtr
                 return (Left $ Error (Just DatabaseOpeningError) err)
         else do managed <- newForeignPtr cx_database_delete dbHandle
                 return (Right $ WritableDatabase $ Database managed)

-- * document handling

-- | @addDocument db doc@ will add the document @doc@ to the writable
-- open database @db@.
addDocument :: (Serialize dat, Prefixable fields)
            => WritableDatabase fields dat
            -> Document fields dat
            -> IO DocumentId
addDocument = undefined


                
deleteDocumentById :: (Serialize dat, Prefixable fields)
                   => Database fields dat -> DocumentId -> IO ()
deleteDocumentById = undefined

deleteDocumentByTerm :: (Serialize dat, Prefixable fields)
                     => Database fields dat -> Term -> IO ()
deleteDocumentByTerm = undefined
     

replaceDocument :: (Serialize dat, Prefixable fields)
                => Database fields dat
                -> DocumentId
                -> Document fields dat
                -> IO ()
replaceDocument = undefined
