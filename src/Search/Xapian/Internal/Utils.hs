module Search.Xapian.Internal.Utils
     ( -- * General
       collect
     , collectTerms
     , collectDocIds

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
import Foreign.C.String
import Blaze.ByteString.Builder as Blaze
import Data.Monoid
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, ByteString, packCString, useAsCString)
import Data.Serialize

import Search.Xapian.Internal.Types
import Search.Xapian.Types
import Search.Xapian.Internal.FFI

-- * General

-- | @collect@ returns objects using a triple of functions over iterators
-- @(finished, next, get)@ and two pointers @pos@ and @end@ where
-- 
-- @pos@ points to the current iterator position
-- 
-- @end@ denotes the end of the iterator
--
-- @finished@ checks whether there are elements
-- left in the iterator
--
-- @next@ moves the @pos@ to the next element
-- 
-- @get@ converts the pointer to some meaningful 'object' performing an
-- effectful computation
collect :: (Ptr a -> IO ()) -- next
        -> (Ptr a -> IO b)  -- get
        -> (Ptr a -> Ptr a -> IO Bool) -- finished?
        -> Ptr a -- current position
        -> Ptr a -- end
        -> IO [b]
collect next get finished pos end =
 do exit <- finished pos end
    if exit
       then do return []
       else do elem <- get pos
               next pos
               rest <- collect next get finished pos end
               return (elem : rest)


collectTerms :: Ptr CTermIterator -- current position
             -> Ptr CTermIterator -- end
             -> IO [CString]
collectTerms = collect cx_termiterator_next
                       cx_termiterator_get
                       cx_termiterator_is_end

collectDocIds :: Ptr CMSetIterator
              -> Ptr CMSetIterator
              -> IO [DocumentId]
collectDocIds = collect cx_msetiterator_next
                        (fmap DocId . cx_msetiterator_get)
                        cx_msetiterator_is_end

-- * Document related

newDocumentPtr :: IO DocumentPtr
newDocumentPtr =
 do cx_document_new >>= manage

-- | @addPosting document posting pos@ will index the term @posting@ in
-- the document @document@ at position @pos@.
addPosting' :: DocumentPtr   -- ^ The document to add a posting to
            -> ByteString -- ^ The term to index within the document
            -> Pos           -- ^ The position of the term within the document
            -> IO ()
addPosting' docFPtr term pos =
    withForeignPtr docFPtr $ \docPtr ->
    BS.useAsCString term   $ \cterm ->
    cx_document_add_posting
        docPtr
        cterm
        (fromIntegral pos) -- FIXME
        1 -- FIXME

addTerm' :: DocumentPtr
         -> ByteString
         -> IO ()
addTerm' docFPtr term =
    withForeignPtr docFPtr $ \docPtr ->
    useAsCString term $ \cterm ->
    cx_document_add_term
        docPtr
        cterm
        1 -- FIXME

addValue :: DocumentPtr -> ValueNumber -> Value -> IO ()
addValue docFPtr valno val =
    useAsCString val $ \cval ->
    withForeignPtr docFPtr $ \docPtr ->
    cx_document_add_value docPtr (fromIntegral valno) cval

getDocumentTerms :: DocumentPtr -> IO [Term]
getDocumentTerms docFPtr =
    withForeignPtr docFPtr $ \docPtr ->
     do begin <- cx_document_termlist_begin docPtr
        end   <- cx_document_termlist_end docPtr
        cterms <- collectTerms begin end
        mapM (fmap Term . BS.packCString) cterms

setDocumentData :: Serialize dat
                => DocumentPtr
                -> dat
                -> IO ()
setDocumentData docFPtr docData =
    withForeignPtr docFPtr $ \docPtr ->
    BS.useAsCString (unnullify $ encode docData) $ \encodedData ->
        cx_document_set_data docPtr encodedData

getDocumentData :: Serialize dat
                => DocumentPtr
                -> IO (Either Error dat)
getDocumentData docFPtr =
    withForeignPtr docFPtr $ \docPtr ->
     do dat <- BS.packCString =<< cx_document_get_data docPtr
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
               -> ForeignPtr CDocument  -- ^ The document to add terms to
               -> ByteString    -- ^ The text to stem and index
               -> IO ()
stemToDocument stemmer document text =
  undefined  -- need to write the FFI to Xapian::TermGenerator first

stemWord :: StemPtr -> ByteString -> IO ByteString
stemWord stemFPtr word =
    withForeignPtr stemFPtr $ \stemPtr ->
    useAsCString word $ \cword ->
    cx_stem_word stemPtr cword >>= packCString


createStemmer :: Stemmer -> IO StemPtr
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
        do cx_stem_new_with_language clang
           >>= newForeignPtr cx_stem_delete
