module Search.Xapian.Document
     ( -- * Constructor
       document
     , simpleDocument
     
       -- * Terms, Fields, and Postings
     , addTerm, addTerms
     , addPosting, addPostings
     , addField, addFields, getField
     ) where


import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize

import Search.Xapian.Types

document :: (Serialize dat, Prefixable fields) => dat -> Document fields dat
document t = Document Nothing Nothing Nothing [] Map.empty t

simpleDocument :: Serialize dat => dat -> Document Fieldless dat
simpleDocument = document

addTerm :: Serialize dat => ByteString
        -> Document fields dat -> Document fields dat
addTerm bs_term = addTerms [bs_term]

addTerms :: Serialize dat => [ByteString]
         -> Document fields dat -> Document fields dat
addTerms bs_terms doc =
    doc{documentTerms = map Term bs_terms ++ documentTerms doc}

addPosting :: Serialize dat => Pos -> ByteString
           -> Document fields dat -> Document fields dat
addPosting pos posting = addPostings [(pos, posting)]


addPostings :: Serialize dat => [(Pos, ByteString)]
            ->  Document fields dat -> Document fields dat
addPostings postings doc = 
    doc{documentTerms = map (uncurry Posting) postings ++ documentTerms doc}

addField :: (Serialize dat, Prefixable fields)
         => fields -> ByteString -> Document fields dat -> Document fields dat
addField field value doc =
    doc {documentFields = Map.insert field value $ documentFields doc}

addFields :: (Serialize dat, Prefixable fields)
          => Map fields ByteString -> Document fields dat -> Document fields dat
addFields fieldsMap doc =
    doc {documentFields = Map.union fieldsMap $ documentFields doc}
    -- assuming `Map.union = Map.unionWith const`

getField :: (Serialize dat, Prefixable fields)
         => fields -> Document fields dat -> Maybe ByteString
getField field doc =
    Map.lookup field $ documentFields doc
