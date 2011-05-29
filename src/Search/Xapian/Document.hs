module Search.Xapian.Document
     ( emptyDocument
       -- * Data, Terms, Postings and Values
     , setData, getData
     , addTerm, getTerms, delTerm, clearTerms
     , addPosting, delPosting
     , getValue, setValue, delValue, clearValues
     , getValues, setValues
     , indexText
     ) where

import Foreign
 
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (ByteString, useAsCString)
import qualified Data.ByteString.Char8 as BS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Search.Xapian.Types
import Search.Xapian.Internal.Utils
import Search.Xapian.Internal.FFI

withDocumentPtr :: (Ptr CDocument -> IO a) -> Document -> XapianM a
withDocumentPtr action doc = liftIO $ withForeignPtr (docPtr doc) action

emptyDocument :: XapianM Document
emptyDocument =
 do ptr <- liftIO $ manage =<< cx_document_new
    return (Document ptr (DocId 0))

getData :: Document -> XapianM ByteString
getData =
    withDocumentPtr $ \ptr ->
    cx_document_get_data ptr >>= manage >>= fromCCString
        

setData :: ByteString -> Document -> XapianM ()
setData dat =
    withDocumentPtr $ \ptr ->
    useAsCCString dat $ \ccdat ->
    cx_document_set_data ptr ccdat

getTerms :: Document -> XapianM [Term]
getTerms =
    withDocumentPtr $ \ptr ->
     do b <- manage =<< cx_document_termlist_begin ptr
        e <- manage =<< cx_document_termlist_end ptr
        collectTerms b e

addTerm :: ByteString -> Document -> XapianM ()
addTerm term =
    withDocumentPtr $ \ptr ->
    useAsCString term $ \cterm ->
    cx_document_add_term ptr cterm 1

delTerm :: ByteString -> Document -> XapianM ()
delTerm term =
    withDocumentPtr $ \ptr ->
    useAsCString term $ \cterm ->
    cx_document_remove_term ptr cterm

clearTerms :: Document -> XapianM ()
clearTerms =
    withDocumentPtr $ \ptr ->
    cx_document_clear_terms ptr

addPosting :: Int -> ByteString -> Document -> XapianM ()
addPosting pos term =
    withDocumentPtr $ \ptr ->
    useAsCString term $ \cterm ->
    cx_document_add_posting ptr cterm (fromIntegral pos) 1

delPosting :: Int -> ByteString -> Document -> XapianM ()
delPosting pos term =
    withDocumentPtr $ \ptr ->
    useAsCString term $ \cterm ->
    cx_document_remove_posting ptr cterm (fromIntegral pos) 1

getValue :: ValueNumber -> Document -> XapianM Value
getValue valno =
    withDocumentPtr $ \ptr ->
    cx_document_get_value ptr (fromIntegral valno) >>= BS.packCString

getValues :: Document -> XapianM (IntMap Value)
getValues =
    withDocumentPtr $ \ptr ->
     do b <- manage =<< cx_document_values_begin ptr
        e <- manage =<< cx_document_values_end   ptr
        fmap IntMap.fromList $ collectValues b e

setValue :: ValueNumber -> Value -> Document -> XapianM ()
setValue valno val =
    withDocumentPtr $ \ptr ->
    useAsCString val $ \cval ->
    cx_document_add_value ptr (fromIntegral valno) cval

setValues :: IntMap Value -> Document -> XapianM ()
setValues vals doc =
    forM_ (IntMap.toList vals) $ \(valno, val) ->
    setValue (fromIntegral valno) val doc

delValue :: ValueNumber -> Document -> XapianM ()
delValue valno =
    withDocumentPtr $ \ptr ->
    cx_document_remove_value ptr (fromIntegral valno)

clearValues :: Document -> XapianM ()
clearValues =
    withDocumentPtr cx_document_clear_values

indexText :: Stemmer -> ByteString -> Document -> XapianM ()
indexText stemmer text =
    withDocumentPtr $ \ptr ->
    indexToDocument ptr (Just stemmer) text
