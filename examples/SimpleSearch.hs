import Search.Xapian
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as BS
import System (getArgs, getProgName)
import qualified Data.List as List
import Control.Monad (forM_)


main =
 do args <- getArgs
    progName <- getProgName
    case args of
         path:query -> do Right db <- openReadOnly path
                          doQuery db query
         _      -> putStrLn $ "Usage: "
                            ++ progName
                            ++ " PATH_TO_DATABASE QUERY"

doQuery db queryStrings =
 do --let q = rawQuery English (unwords queryStrings)
    let q = queryAny queryStrings
    print q
    results <- runXapian $
     do (mset,doclist) <- search db q (paging 0 10)
        pars <- mapM getData doclist
        return (zip doclist pars)
    forM_ results $ \(doc, par) ->
     do putStrLn $ "# " ++ (show $ getDocId $ docId doc)
                 ++ " -----------------------------"
        BS.putStrLn par
