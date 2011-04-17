module Search.Xapian.Database where

import Foreign
import qualified Data.ByteString as BS
import Data.Serialize
import Search.Xapian.Document
import Search.Xapian.Types

-- * classes and types


-- * opening databases

openDatabase :: Serialize doc
             => FilePath
             -> IO (Either String (Database doc))
openDatabase = undefined

openWritableDatabase :: Serialize doc
                     => InitDBOption
                     -> FilePath
                     -> IO (Either String (WritableDatabase doc))
openWritableDatabase = undefined

-- * querying databases


search :: (Serialize doc, ReadableDatabase db)
       => db doc
       -> Query
       -> IO [DocumentId]
search = searchWith undefined


-- * query objects


mkQuery :: BS.ByteString -> Query
mkQuery = Query

compileQuery :: Query -> IO CompiledQuery
compileQuery q =
  case q of
       EmptyQuery -> undefined
       Query bs   -> undefined bs
       And q' q'' -> undefined q' q''
       Or  q' q'' -> undefined q' q''

queryAll :: [Query] -> Query
queryAll = foldr And EmptyQuery

queryAny :: [Query] -> Query
queryAny = foldr Or EmptyQuery
