module Search.Xapian.Database where

import Foreign
import Foreign.C.String
import Control.Monad (forM_)
import Control.Applicative
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, useAsCString)
import Data.Either (rights)
import Data.Serialize

import Search.Xapian.Types
import Search.Xapian.Internal.Types
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
              MSet . rights <$> fetchMSets msets
    where
      fetchMSets msets =
          if c_xapian_msets_valid msets
             then do mset <- c_xapian_msets_get msets
                     let docId = DocId (fromIntegral mset)
                     doc  <- getDocument database docId
                     c_xapian_msets_next msets
                     rest <- fetchMSets msets
                     return (doc:rest)
             else return []
          

  getDocument (Database database) docId@(DocId id') =
       withForeignPtr database $ \dbPtr ->
       handleError dbPtr $ \docPtr ->
        do docFPtr <- newForeignPtr c_xapian_document_delete docPtr
           eitherDocDat  <- getDocumentData docFPtr
           case eitherDocDat of
                Left err  -> return (Left err)
                Right dat -> do terms <- getDocumentTerms docFPtr
                                return . Right $ Document (Just docId) dat terms
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

openDatabase :: Serialize doc
             => FilePath
             -> IO (Either Error (Database doc))
openDatabase path =
  useAsCString (pack path) $ \cpath ->
  alloca $ \errorPtr ->
   do dbHandle <- c_xapian_database_new cpath errorPtr
      if dbHandle == nullPtr
         then do err <- peekCString =<< peek errorPtr
                 return (Left $ Error (Just GenericError) err)
         else do managed <- newForeignPtr c_xapian_database_delete dbHandle
                 return (Right $ Database managed)

openWritableDatabase :: Serialize doc
                     => InitDBOption
                     -> FilePath
                     -> IO (Either Error (WritableDatabase doc))
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

addDocument :: Serialize t
            => WritableDatabase t
            -> Document t
            -> IO DocumentId
addDocument (WritableDatabase (Database db)) doc =
 do docFPtr <- newDocumentPtr 
    withForeignPtr db $ \dbPtr ->
     do forM_ (documentTerms doc) $ \term ->
            case term of
                 Term        term' -> addTerm' docFPtr term'
                 Posting pos term' -> addPosting' docFPtr term' pos
        setDocumentData docFPtr (documentData doc)
        withForeignPtr docFPtr $ \docPtr ->
            DocId . fromIntegral <$> c_xapian_database_add_document dbPtr docPtr
                
deleteDocumentById :: Serialize t => Database t -> DocumentId -> IO ()
deleteDocumentById = undefined

deleteDocumentByTerm :: Serialize t => Database t -> Term -> IO ()
deleteDocumentByTerm = undefined
     

replaceDocument :: Serialize t
                => Database t
                -> DocumentId
                -> Document t
                -> IO ()
replaceDocument = undefined
