module Search.Xapian.Database where

import Prelude hiding (words)
import Foreign
import Foreign.C.String
import Control.Monad (forM_)
import Control.Applicative
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
import Search.Xapian.FFI
import qualified Search.Xapian.Query as Q


-- * interfacing the database

instance ReadableDatabase Database where
  search database@(Database dbFPtr) query (QueryRange off lim) =
   do queryFPtr <- Q.compileQuery query
      withForeignPtr dbFPtr $ \dbPtr ->
          withForeignPtr queryFPtr $ \queryPtr ->
           do enquire <- c_xapian_enquire_new dbPtr
              let msets = c_xapian_enquire_query enquire queryPtr off lim
              MSet . rights <$> collect c_xapian_msets_valid
                                        c_xapian_msets_next
                                        get_as_document
                                        msets
    where
      get_as_document msets =
       do mset <- c_xapian_msets_get msets
          let docId = DocId $ fromIntegral mset
          getDocument database docId
          

  getDocument (Database database) docId@(DocId id') =
       withForeignPtr database $ \dbPtr ->
       handleError dbPtr $ \docPtr ->
        do docFPtr <- newForeignPtr c_xapian_document_delete docPtr
           eitherDocDat  <- getDocumentData docFPtr
           case eitherDocDat of
                Left err  -> return (Left err)
                Right dat ->
                 do terms <- getDocumentTerms docFPtr
                    return . Right $
                        (document dat){documentTerms = terms
                                      , documentId = Just docId}
    where
      handleError dbPtr action =
        alloca $ \errorPtr ->
         do handle <- c_xapian_get_document dbPtr (fromIntegral id') errorPtr
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
   do dbHandle <- c_xapian_database_new cpath errorPtr
      if dbHandle == nullPtr
         then do err <- peekCString =<< peek errorPtr
                 return (Left $ Error (Just GenericError) err)
         else do managed <- newForeignPtr c_xapian_database_delete dbHandle
                 return (Right $ Database managed)

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
      dbHandle <- c_xapian_writable_db_new cpath option' errorPtr
      if dbHandle == nullPtr
         then do err <- peekCString =<< peek errorPtr
                 return (Left $ Error (Just GenericError) err)
         else do managed <- newForeignPtr c_xapian_database_delete dbHandle
                 return (Right $ WritableDatabase $ Database managed)

-- * document handling

-- | @addDocument db doc@ will add the document @doc@ to the writable
-- open database @db@.
addDocument :: (Serialize dat, Prefixable fields)
            => WritableDatabase fields dat
            -> Document fields dat
            -> IO DocumentId
addDocument (WritableDatabase (Database db))
            doc@Document{documentStem = maybeStemmer} =
 do docFPtr <- newDocumentPtr 
    withForeignPtr db $ \dbPtr ->
     do stemFPtr <- sequenceA $ createStemmer <$> maybeStemmer
        add_terms docFPtr stemFPtr
        add_values docFPtr (documentValues doc)
        add_fields docFPtr (documentFields doc)
        setDocumentData docFPtr (documentData doc)
        withForeignPtr docFPtr $ \docPtr ->
            DocId . fromIntegral <$> c_xapian_database_add_document dbPtr docPtr
  where
    stem maybeStemFPtr word =
      case maybeStemFPtr of
           Just stemFPtr -> stemWord stemFPtr word
           Nothing -> return word

    add_terms docFPtr stemFPtr =
     do forM_ (documentTerms doc) $ \term ->
          case term of
               Term        term' -> stem stemFPtr term' >>= addTerm' docFPtr
               Posting pos term' -> stem stemFPtr term' >>= (\term'' ->
                                    addPosting' docFPtr term'' pos)
               RawText bs        ->
                  case maybeStemmer of
                       Just stemmer -> stemToDocument stemmer docFPtr bs
                       Nothing      -> mapM_ (addTerm' docFPtr) (words bs)

    add_values docFPtr values =
        mapM_ (uncurry $ addValue docFPtr) (IntMap.toList values)

    add_fields docFPtr fields =
        forM_ (Map.toList fields) $ \(key, values) ->
            forM_ values $ \value ->
                addTerm' docFPtr (getPrefix key `BS.append` value)


                
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
