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
     ) where


import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Serialize

import Search.Xapian.Types
import Search.Xapian.Internal.Types

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

addPosting :: ByteString -> Int -> Document fields dat -> Document fields dat
addPosting term pos = queueDiff $ AddPosting term pos 1

addPostings :: [(ByteString, Int)] -> Document fields dat -> Document fields dat
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
