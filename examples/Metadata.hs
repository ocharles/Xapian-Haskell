import Search.Xapian
import Data.ByteString.Char8 (pack)
import System (getArgs)

helptext = unlines ["Usage: PROG_NAME get PATH_TO_DATABASE KEY"
                   ,"       PROG_NAME set PATH_TO_DATABASE KEY VALUE"
                   ]

set path key value =
 do Right db <- openReadWrite Open path
    runXapian $ setMetadata db key value

get path key =
 do Right db <- openReadWrite Open path
    runXapian $ getMetadata db key

main =
 do args <- getArgs
    case args of
         "get" : path : key : _ ->
             get path (pack key) >>= print
         "set" : path : key : value : _ ->
             set path (pack key) (pack value) >>= print
         _  -> putStrLn helptext

