module Search.Xapian.Internal.Utils
     ( -- * General
       collect

       -- * Document related
     , newDocumentPtr
     , addPosting'
     , addTerm'
     , getDocumentTerms
     , getDocumentData
     , setDocumentData
     ) where

import Foreign
import Foreign.C.String
import Blaze.ByteString.Builder as Blaze
import Blaze.ByteString.Builder.ByteString as Blaze
import Data.Bits
import Data.Monoid
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, ByteString)
import Data.Serialize

import Search.Xapian.Internal.Types
import Search.Xapian.Types
import Search.Xapian.FFI

-- * General

-- | @collect@ returns objects using a triple of functions over iterators
-- @(continue, next, get)@ and a pointer @iterPtr@ where
-- 
-- @iterPtr@ points to the first element of the iterator
--
-- @continue@ checks whether there are elements
-- left in the iterator,
--
-- @next@ moves the @iterPtr@ to the next element
-- 
-- @get@ converts the pointer to some meaningful 'object' performing an
-- effectful computation

collect :: (Ptr a -> Bool) -> (Ptr a -> IO ()) -> (Ptr a -> IO b) -> (Ptr a -> IO [b])
collect continue next get iterPtr =
  if continue iterPtr
     then do element <- get iterPtr
             next iterPtr
             rest <- collect continue next get iterPtr
             return (element : rest)
     else do return []


-- * Document related

newDocumentPtr :: IO DocumentPtr
newDocumentPtr =
 do document <- c_xapian_document_new
    newForeignPtr c_xapian_document_delete document

-- | @addPosting document posting pos@ will index the term @posting@ in
-- the document @document@ at position @pos@.
addPosting' :: DocumentPtr   -- ^ The document to add a posting to
            -> ByteString -- ^ The term to index within the document
            -> Pos           -- ^ The position of the term within the document
            -> IO ()
addPosting' docFPtr term pos =
    withForeignPtr docFPtr $ \docPtr ->
    BS.useAsCString term   $ \dat ->
    c_xapian_document_add_posting docPtr dat (fromIntegral pos) -- fix it

addTerm' :: DocumentPtr
         -> ByteString
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
unnullify :: ByteString -> ByteString
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
nullify :: ByteString -> ByteString
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
