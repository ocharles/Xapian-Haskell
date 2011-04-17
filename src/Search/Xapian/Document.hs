module Search.Xapian.Document where


import Foreign
import qualified Data.ByteString as BS
import Data.Serialize

import Search.Xapian.Types

addTerm :: Serialize t => BS.ByteString -> Document t -> Document t
addTerm bs doc@Document{documentTerms = documentTerms} =
    doc{documentTerms = Term bs : documentTerms}

addPosting :: Serialize t => Pos -> BS.ByteString -> Document t -> Document t
addPosting pos bs doc@Document{documentTerms = documentTerms} =
      doc{documentTerms = Posting pos bs : documentTerms}
