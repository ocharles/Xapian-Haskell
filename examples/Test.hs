module Test where

import Search.Xapian

--
-- For testing - here's how it looks to clients
--
testStuff =
  do let textData = "Oh my, how very confusing Haskell can be!"
     (Right db) <- openWritableDatabase "test.db" createOrOverwriteDB
     doc <- newDocument
     stemToDocument englishStem doc textData
     setDocumentData doc textData
     addDocument db doc
     return ()

testQuery =
  do (Right db) <- openDatabase "test.db"
     enquire db (query "red" <&> query "red") 0 10
