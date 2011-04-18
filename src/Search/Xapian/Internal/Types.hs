-- | this module is not intended to be made visible
module Search.Xapian.Internal.Types
       ( Error (..)
       , NativeError (..)

       , WritableDatabase (..)
       , Database (..)

       , Query (..)
       , OpNullary (..)
       , OpUnary (..)
       , OpBinary (..)
       , OpMulti (..)
       , QueryPtr

       , Pos
       , DocumentPtr
       , Value
       , ValueNumber
       , newDocumentPtr
       , addTerm'
       , addPosting'
       , setDocumentData
       , getDocumentData
       , Term (..)
       , getDocumentTerms
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
import Search.Xapian.FFI

-- | Error, inspired (blatantly copied) by hdbc
--
-- The main Xapian exception object. As much information as possible is passed from
-- the database through to the application through this object.
--
-- Errors generated in the Haskell layer will have seNativeError set to
-- Nothing.
-- 



data Error = Error { xeNativeError :: Maybe NativeError
                   , seErrorMsg :: String
                   } deriving (Show)

data NativeError = DocNotFoundError | GenericError
  deriving (Eq, Show)


-- * Database related types
-- --------------------------------------------------------------------

data Database t = Database !(ForeignPtr XapianDatabase)
  deriving (Eq, Show)

newtype WritableDatabase t = WritableDatabase (Database t)


-- * Query related types
-- --------------------------------------------------------------------

type QueryPtr = ForeignPtr XapianEnquire

-- Internal Representation of Queries

data Query
    = EmptyQuery -- ^ does not match anything
    | Atom ByteString
    | Nullary OpNullary
    | Unary  OpUnary   Query
    | Binary OpBinary  Query  Query
    | Multi  OpMulti  [Query]
    deriving (Show)

data OpNullary
    = OpValueGE ValueNumber Value
    | OpValueLE ValueNumber Value
    | OpValueRange ValueNumber [Value]
    deriving (Show)

data OpUnary
    = OpScaleWeight Double -- Xapian::InvalidArgumentError if scale is negative
    deriving (Show)

data OpBinary
    = OpOr 
    | OpEliteSet
    | OpAnd 
    | OpXor 
    | OpAndMaybe 
    | OpAndNot 
    | OpFilter 
    | OpNear Int
    deriving (Show)

data OpMulti
    = OpSynonym
    | OpPhrase Int
    deriving (Show)


-- * Document related types
-- --------------------------------------------------------------------

type DocumentPtr = ForeignPtr XapianDocument

type Pos  = Word32

-- * document fields
type ValueNumber = Int
type Value       = ByteString

-- * handling documents

data Term = Term ByteString | Posting Pos ByteString
  deriving (Eq, Show)

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


-- * for iteration

collect :: (Ptr a -> Bool) -> (Ptr a -> IO ()) -> (Ptr a -> IO b) -> (Ptr a -> IO [b])
collect continue next get iterPtr =
  if continue iterPtr
     then do element <- get iterPtr
             next iterPtr
             rest <- collect continue next get iterPtr
             return (element : rest)
     else do return []
