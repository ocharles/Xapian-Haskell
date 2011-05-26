module Search.Xapian.Document
     ( emptyDocument
       -- * Data, Terms, Postings and Values
     , setData, getData
     , addTerm, getTerms, delTerm, clearTerms
     , addPosting, delPosting
     , getValue, setValue, delValue, clearValues
     , indexText
     ) where

import Foreign

import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (ByteString, useAsCString)
import qualified Data.ByteString.Char8 as BS

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
    cx_document_get_data ptr >>= BS.packCString

setData :: ByteString -> Document -> XapianM ()
setData dat =
    withDocumentPtr $ \ptr ->
    useAsCString dat $ \cdat ->
    cx_document_set_data ptr cdat

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

setValue :: ValueNumber -> Value -> Document -> XapianM ()
setValue valno val =
    withDocumentPtr $ \ptr ->
    useAsCString val $ \cval ->
    cx_document_add_value ptr (fromIntegral valno) cval

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
