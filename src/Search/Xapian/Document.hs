module Search.Xapian.Document
     ( -- * Constructor
       document
     , simpleDocument
     
       -- * Terms, Fields, and Postings
     , addTerm, addTerms, getTerms
     , addPosting, addPostings
     , addField, addFields, getField
     , getValue, setValue
     , clearValues, clearTerms
     , fieldsFromTerms

       -- * Low level
     , applyAccumulatedChanges
     ) where

import Foreign

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.ByteString.Char8 (ByteString, useAsCString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Sequence ((|>), ViewL (..))
import qualified Data.Sequence as Seq
import Data.Serialize

import Search.Xapian.Types
import Search.Xapian.Internal.Types
import Search.Xapian.Internal.Utils
import Search.Xapian.Internal.FFI

-- | @document@ constructs a @Document@ which contains only document data.
document :: (Serialize dat, Prefixable fields) => dat -> Document fields dat
document dat = Document Nothing Nothing Nothing IntMap.empty [] Map.empty dat Seq.empty

-- | @simpleDocument@ constructs a @SimpleDocument@ which contains only
-- document data. A SimpleDocument does not contain any fields.
simpleDocument :: Serialize dat => dat -> SimpleDocument dat
simpleDocument = document

queueDiff :: DocumentDiff fields dat
          -> (Document fields dat -> Document fields dat)
queueDiff diff =
  \ doc@Document{documentDiffs = queue} -> doc {documentDiffs = queue |> diff}


-- FIXME: this does not return terms manually added ... for now
getTerms :: Document fields dat -> [Term]
getTerms = documentLazyTerms

addTerm :: ByteString -> Document fields dat -> Document fields dat
addTerm term = queueDiff $ AddTerm term 1

addTerms :: [ByteString] -> Document fields dat -> Document fields dat
addTerms terms = queueDiff $ AddTerms (map (\x->(x,1)) terms)

removeTerm = undefined
removeTerms = undefined
clearTerms = queueDiff ClearTerms

addPosting :: ByteString -> Word32 -> Document fields dat -> Document fields dat
addPosting term pos = queueDiff $ AddPosting term pos 1

addPostings :: [(ByteString, Word32)] -> Document fields dat -> Document fields dat
addPostings terms = queueDiff $ AddPostings (map (\(term,pos) -> (term,pos,1)) terms)

removePosting = undefined
removePostings = undefined

addField :: Prefixable field => field -> ByteString
         -> Document fields dat -> Document fields dat
addField field value = addTerm $ getPrefix field `BS.append` value

addFields :: Prefixable field => Map field ByteString
          -> Document fields dat -> Document fields dat
addFields fieldsMap = addTerms $ map mapper $ Map.toList fieldsMap
    where mapper (field, value) = getPrefix field `BS.append` value

getField :: Prefixable fields => fields -> Document fields dat -> Maybe [ByteString]
getField field doc = Map.lookup field $ documentLazyFields doc

getValue = undefined
setValue = undefined
removeValue = undefined
clearValues = queueDiff ClearValues

-- FIXME: I am a stupid algorithm :D
fieldsFromTerms :: Prefixable fields => [Term] -> Map fields [ByteString]
fieldsFromTerms = Map.fromListWith (++) . go
  where
    go (Term term []:rest) =
        head [(f, [fromJust $ stripPrefix f term]) | (f,pre) <- prefixes, pre `BS.isPrefixOf` term] :
        go rest
    go (_:rest) = go rest
    go [] = []
    prefixes = map (id &&& getPrefix) allFields

-- Low Level
-- ------------------------------------------------------------------


-- copies the internally used @DocumentPtr@ and applies the changes
applyAccumulatedChanges :: Serialize dat => Document fields dat -> IO DocumentPtr
applyAccumulatedChanges document =
 do docFPtr' <- case documentPtr document of
                     Nothing      -> manage =<< cx_document_new
                     Just docFPtr -> withForeignPtr docFPtr $ \docPtr ->
                                         manage =<< cx_document_copy docPtr
    withForeignPtr docFPtr' $ \docPtr ->
        go (documentDiffs document) docPtr
    return docFPtr'
  where
    go seq docFPtr =
        case Seq.viewl seq of
             EmptyL       -> return ()
             diff :< seq' -> execute diff docFPtr >> go seq' docFPtr

    execute diff docPtr =
        case diff of
             AddTerm term wdfinc ->
                 useAsCString term $ \cterm ->
                 cx_document_add_term docPtr cterm wdfinc
             DelTerm term ->
                 useAsCString term $ \cterm ->
                 cx_document_remove_term docPtr cterm
             AddPosting term pos wdfinc ->
                 useAsCString term $ \cterm ->
                 cx_document_add_posting docPtr cterm pos wdfinc
             DelPosting term pos wdfdec ->
                 useAsCString term $ \cterm ->
                 cx_document_remove_posting docPtr cterm pos wdfdec
             AddTerms terms ->
                 forM_ terms $ \(term,wdfinc) ->
                 useAsCString term $ \cterm ->
                 cx_document_add_term docPtr cterm wdfinc
             DelTerms terms ->
                 forM_ terms $ \term ->
                 useAsCString term $ \cterm ->
                 cx_document_remove_term docPtr cterm
             AddPostings terms ->
                 forM_ terms $ \(term,pos,wdfinc) ->
                 useAsCString term $ \cterm ->
                 cx_document_add_posting docPtr cterm pos wdfinc
             DelPostings terms ->
                 forM_ terms $ \(term,pos,wdfdec) ->
                 useAsCString term $ \cterm ->
                 cx_document_remove_posting docPtr cterm pos wdfdec
             AddRawText _ ->
                 undefined -- | TODO: write termgenerator FFI first
             SetData dat ->
                 useAsCString (unnullify $ encode dat) $ \cDat ->
                 cx_document_set_data docPtr cDat
             ClearTerms -> cx_document_clear_terms docPtr
             ClearValues -> cx_document_clear_values docPtr
