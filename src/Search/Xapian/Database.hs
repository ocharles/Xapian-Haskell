module Search.Xapian.Database where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Control.Monad (forM_)
import Control.Applicative
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, useAsCString)
import Data.Serialize

import Search.Xapian.Document
import Search.Xapian.Types
import Search.Xapian.Internal.Types
import Search.Xapian.FFI


-- * opening databases

openDatabase :: Serialize doc
             => FilePath
             -> IO (Either XapianError (Database doc))
openDatabase path =
  useAsCString (pack path) $ \cpath ->
  alloca $ \errorPtr ->
   do dbHandle <- c_xapian_database_new cpath errorPtr
      if dbHandle == nullPtr
         then do err <- peekCString =<< peek errorPtr
                 return (Left $ XapianError (Just undefined) err)
         else do managed <- newForeignPtr c_xapian_database_delete dbHandle
                 return (Right $ Database managed)

openWritableDatabase :: Serialize doc
                     => InitDBOption
                     -> FilePath
                     -> IO (Either XapianError (WritableDatabase doc))
openWritableDatabase option path =
  useAsCString (pack path) $ \cpath ->
  alloca $ \errorPtr ->
   do let option' = packInitDBOption option
      dbHandle <- c_xapian_writable_db_new cpath option' errorPtr
      if dbHandle == nullPtr
         then do err <- peekCString =<< peek errorPtr
                 return (Left $ XapianError (Just undefined) err)
         else do managed <- newForeignPtr c_xapian_database_delete dbHandle
                 return (Right $ WritableDatabase $ Database managed)


-- * querying databases


search :: (Serialize doc, ReadableDatabase db)
       => db doc
       -> Query
       -> IO (MSet doc)
search db query = searchWith db query undefined


-- * query objects


mkQuery :: BS.ByteString -> Query
mkQuery = Query

queryAll :: [Query] -> Query
queryAll = foldr And EmptyQuery

queryAny :: [Query] -> Query
queryAny = foldr Or EmptyQuery

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
                
deleteDocument :: Serialize t => Database t -> DocumentId -> IO ()
deleteDocument = undefined

deleteDocumentByTerm :: Serialize t => Database t -> Term -> IO ()
deleteDocumentByTerm = undefined
     

replaceDocument :: Serialize t
                => Database t
                -> DocumentId
                -> Document t
                -> IO ()
replaceDocument = undefined
