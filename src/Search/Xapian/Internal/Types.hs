-- | this module is not intended to be made visible
module Search.Xapian.Internal.Types
       ( Error (..)
       , WritableDatabase (..)
       , Database (..)
       , Pos
       , DocumentPtr
       , newDocumentPtr
       , addTerm'
       , addPosting'
       , setDocumentData
       , getDocumentData
       , Query (..)
       , QueryPtr
       , queryString
       , compileQuery
       , Term (..)
       , getDocumentTerms
       , NativeError (..)
       ) where

import Foreign
import Foreign.C.String
import Blaze.ByteString.Builder as Blaze
import Blaze.ByteString.Builder.ByteString as Blaze
import Data.Bits
import Data.Monoid
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Serialize
import Search.Xapian.FFI

data Error = Error { xeNativeError :: Maybe NativeError
                   , seErrorMsg :: String
                   } deriving (Show)

data NativeError = DocNotFoundError | GenericError
  deriving (Eq, Show)

data Database t = Database !(ForeignPtr XapianDatabase)
  deriving (Eq, Show)

newtype WritableDatabase t = WritableDatabase (Database t)

type DocumentPtr = ForeignPtr XapianDocument

type Pos  = Word32


-- * building queries

data Query = EmptyQuery
           | Query BS.ByteString
           | Or Query Query
           | And Query Query
  deriving (Show)

type QueryPtr = ForeignPtr XapianEnquire

queryString :: String -> Query
queryString = Query . pack

compileQuery :: Query -> IO QueryPtr
compileQuery q =
    case q of
         EmptyQuery ->
              do c_xapian_query_empty >>= newForeignPtr c_xapian_query_delete
         Query term  ->
             BS.useAsCString term $ \dat ->
              do c_xapian_query_new dat >>= newForeignPtr c_xapian_query_delete
         And q' q'' ->
              do c'  <- compileQuery q'
                 c'' <- compileQuery q''
                 combineQueries c' c'' andOp
         Or  q' q'' ->
              do c'  <- compileQuery q'
                 c'' <- compileQuery q''
                 combineQueries c' c'' orOp
  where
    andOp = 0
    orOp  = 1

    combineQueries a b operator =
        withForeignPtr a $ \queryA ->
        withForeignPtr b $ \queryB ->
         do query <- c_xapian_query_combine operator queryA queryB
            newForeignPtr c_xapian_query_delete query

-- * handling documents

data Term = Term BS.ByteString | Posting Pos BS.ByteString
  deriving (Eq, Show)

newDocumentPtr :: IO DocumentPtr
newDocumentPtr =
 do document <- c_xapian_document_new
    newForeignPtr c_xapian_document_delete document

-- | @addPosting document posting pos@ will index the term @posting@ in
-- the document @document@ at position @pos@.
addPosting' :: DocumentPtr   -- ^ The document to add a posting to
            -> BS.ByteString -- ^ The term to index within the document
            -> Pos           -- ^ The position of the term within the document
            -> IO ()
addPosting' docFPtr term pos =
    withForeignPtr docFPtr $ \docPtr ->
    BS.useAsCString term   $ \dat ->
    c_xapian_document_add_posting docPtr dat (fromIntegral pos) -- fix it

addTerm' :: DocumentPtr
         -> BS.ByteString
         -> IO ()
addTerm' = undefined         

getDocumentTerms :: DocumentPtr -> IO [Term]
getDocumentTerms docFPtr =
    withForeignPtr docFPtr $ \docPtr ->
     do cterms <- collect c_xapian_terms_valid
                          c_xapian_terms_next
                          c_xapian_terms_get
                          (c_xapian_get_terms docPtr)
        mapM (fmap Term . BS.packCString) cterms

setDocumentData :: Serialize dat
                => DocumentPtr
                -> dat
                -> IO ()
setDocumentData docFPtr docData =
    withForeignPtr docFPtr $ \docPtr ->
    BS.useAsCString (unnullify $ encode docData) $ \encodedData ->
        c_xapian_document_set_data docPtr encodedData

getDocumentData :: Serialize dat
                => DocumentPtr
                -> IO (Either Error dat)
getDocumentData docFPtr =
    withForeignPtr docFPtr $ \docPtr ->
     do dat <- BS.packCString =<< c_xapian_document_get_data docPtr
        return $ case decode $ nullify dat of
                      Left msg   -> Left $ Error Nothing msg
                      Right dat' -> Right dat'

-- * handling NULL values
-- because cstrings can't contain any NULL value, we have to store 7 bytes of
-- date as 8 bytes of data

-- | unnullify removes any NULL from the bytestring in order
-- to be able to convert bytestrings into cstrings
unnullify :: BS.ByteString -> BS.ByteString
unnullify = Blaze.toByteString . go
  where
    go bs =
      if BS.null bs
         then mempty
         else
            let (bs',rest) = BS.splitAt 7 bs
                ((_,b),bs'') = BS.mapAccumL flipBit (0,0x80) bs'
            in Blaze.fromStorable (b :: Word8)
               `mappend` Blaze.fromByteString bs''
               `mappend` go rest

    flipBit (pos, acc) byte =
      let acc' = if byte .&. 0x80 == 0 then acc .|. (2^pos) else acc
      in ((pos + 1, acc'), byte .|. 0x80)

-- | nullify is the inverse of unnullify
nullify :: BS.ByteString -> BS.ByteString
nullify = Blaze.toByteString . go
  where
    go bs =
      if BS.null bs
         then mempty
         else
            let (bs',rest) = BS.splitAt 8 bs
                ((_,b),bs'') =
                  BS.mapAccumL flipBit (0, BS.head bs') (BS.tail bs')
            in Blaze.fromByteString bs''
               `mappend` go rest

    flipBit (pos, acc) byte =
      let byte' = if acc .&. 2^pos == 0 then byte else byte - 0x80
      in  ((pos + 1, acc), byte')


-- * for iteration

collect :: (Ptr a -> Bool) -> (Ptr a -> IO ()) -> (Ptr a -> IO b) -> (Ptr a -> IO [b])
collect continue next get iterPtr =
  if continue iterPtr
     then do element <- get iterPtr
             next iterPtr
             rest <- collect continue next get iterPtr
             return (element : rest)
     else do return []
