module Search.Xapian.Document where


import qualified Data.ByteString as BS
import Data.Serialize

import Search.Xapian.Types

addTerm :: Serialize t => BS.ByteString -> Document t -> Document t
addTerm bs_term = addTerms [bs_term]

addTerms :: Serialize t => [BS.ByteString] -> Document t -> Document t
addTerms bs_terms doc@Document{documentTerms = documentTerms'} =
    doc{documentTerms = map Term bs_terms ++ documentTerms'}

addPosting :: Serialize t => Pos -> BS.ByteString -> Document t -> Document t
addPosting pos posting = addPostings [(pos, posting)]


addPostings :: Serialize t => [(Pos, BS.ByteString)] -> Document t -> Document t
addPostings postings doc@Document{documentTerms = documentTerms'} =
      doc{documentTerms = map (uncurry Posting) postings ++ documentTerms'}
