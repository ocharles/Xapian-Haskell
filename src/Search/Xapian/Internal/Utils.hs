module Search.Xapian.Internal.Utils
     ( -- * General
       collect

       -- * Document related
     , newDocumentPtr
     , addPosting'
     , addTerm'
     , addValue
     , getDocumentTerms
     , getDocumentData
     , setDocumentData

       -- * Stemmer related
     , createStemmer
     , stemWord
     , stemToDocument
     ) where

import Foreign
import Blaze.ByteString.Builder as Blaze
import Data.Monoid
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, ByteString, packCString, useAsCString)
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
addTerm' docFPtr term =
    withForeignPtr docFPtr $ \docPtr ->
    useAsCString term $ \cterm ->
    c_xapian_document_add_term docPtr cterm

addValue :: DocumentPtr -> ValueNumber -> Value -> IO ()
addValue docFPtr valno val =
    useAsCString val $ \cval ->
    withForeignPtr docFPtr $ \docPtr ->
    c_xapian_document_add_value docPtr valno cval

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

_zero = 48 :: Word8
z  = 122 :: Word8
z' = BS.pack [z]
z0 = BS.pack [z,_zero]
zz = BS.pack [z,z]

-- | unnullify maps NULL to z0 and z to zz
unnullify :: ByteString -> ByteString
unnullify = Blaze.toByteString . go
  where
    go bs =
      let (xs,xss) = BS.span (\x -> x /= 0 && x /= z) bs
          replacement = if BS.head xss == 0 then z0
                                            else zz
      in if BS.null xss
            then Blaze.fromByteString xs
            else Blaze.fromByteString xs `mappend`
                 Blaze.fromByteString replacement `mappend`
                 go (BS.tail xss)

-- | nullify is the inverse of unnullify
nullify :: ByteString -> ByteString
nullify = Blaze.toByteString . go
  where
    go bs =
        let (xs,xss) = BS.span (/= z) bs
            replacement = if xss `BS.index` 1 == _zero then 0 :: Word8
                                                       else z
        in  if BS.null xss
               then Blaze.fromByteString xs
               else if BS.length xss == 1
                       then error $ "nullify: failed to decode document data" -- FIXME
                       else Blaze.fromByteString xs `mappend`
                            Blaze.fromStorable replacement `mappend`
                            go (BS.drop 2 xss)

-- | @stemToDocument stemmer document text@ adds stemmed posting terms derived from
-- @text@ using the stemming algorith @stemmer@ to @doc@
stemToDocument :: Stemmer      -- ^ The stemming algorithm to use
               -> ForeignPtr XapianDocument  -- ^ The document to add terms to
               -> ByteString    -- ^ The text to stem and index
               -> IO ()
stemToDocument stemmer document text =
    useAsCString text $ \ctext ->
     do stem <- createStemmer stemmer
        withForeignPtr stem $ \stemPtr ->
            withForeignPtr document $ \documentPtr ->
             do c_xapian_stem_string stemPtr documentPtr ctext

stemWord :: StemmerPtr -> ByteString -> IO ByteString
stemWord stemFPtr word =
    withForeignPtr stemFPtr $ \stemPtr ->
    useAsCString word $ \cword ->
    c_xapian_stem_word stemPtr cword >>= packCString


createStemmer :: Stemmer -> IO StemmerPtr
createStemmer stemmer =
    let lang = case stemmer of
                    Danish  -> "danish"
                    Dutch   -> "dutch"
                    DutchKraaijPohlmann -> "kraaij_pohlmann"
                    English -> "english"
                    EnglishLovins -> "lovins"
                    EnglishPorter -> "porter"
                    Finnish -> "finnish"
                    French  -> "french"
                    German  -> "german"
                    German2 -> "german2"
                    Hungarian  -> "hungarian"
                    Italian -> "italian"
                    Norwegian  -> "norwegian"
                    Portuguese -> "portuguese"
                    Romanian -> "romanian"
                    Russian -> "russian"
                    Spanish -> "spanish"
                    Swedish -> "swedish"
                    Turkish -> "turkish"
    in useAsCString (pack lang) $ \clang ->
        do c_xapian_stem_new clang
           >>= newForeignPtr c_xapian_stem_delete
