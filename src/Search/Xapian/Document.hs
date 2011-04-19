module Search.Xapian.Document
     ( -- * Constructor
       document
     , simpleDocument
     
       -- * Terms, Fields, and Postings
     , addTerm, addTerms
     , addPosting, addPostings
     ) where


import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Serialize

import Search.Xapian.Types

document :: (Serialize dat, Prefixable fields) => dat -> Document fields dat
document t = Document Nothing Nothing [] Map.empty t

simpleDocument :: Serialize dat => dat -> Document Fieldless dat
simpleDocument = document

addTerm :: Serialize dat => BS.ByteString
        -> Document fields dat -> Document fields dat
addTerm bs_term = addTerms [bs_term]

addTerms :: Serialize dat => [BS.ByteString]
         -> Document fields dat -> Document fields dat
addTerms bs_terms doc@Document{documentTerms = documentTerms'} =
    doc{documentTerms = map Term bs_terms ++ documentTerms'}

addPosting :: Serialize dat => Pos -> BS.ByteString
           -> Document fields dat -> Document fields dat
addPosting pos posting = addPostings [(pos, posting)]


addPostings :: Serialize dat => [(Pos, BS.ByteString)]
            ->  Document fields dat -> Document fields dat
addPostings postings doc@Document{documentTerms = documentTerms'} =
      doc{documentTerms = map (uncurry Posting) postings ++ documentTerms'}
