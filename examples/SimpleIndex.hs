import Search.Xapian
import Data.ByteString.Char8 (pack)
import System (getArgs, getProgName)
import qualified Data.List as List
import Control.Monad (forM_)


paragraphs =
    map unlines . filter (not . all empty) . List.groupBy p . lines
    where
    empty = all isSpace
    nonempty = not . empty
    isSpace = (`elem` " \t\n\r")
    p l1 l2 = (empty l1 && empty l2) || (nonempty l1 && nonempty l2)


indexParagraphs path pars =
 do Right db <- openReadWrite CreateOrOpen path
    runXapian $ forM_ (map pack pars) $ \par ->
     do doc <- emptyDocument
        setData par doc
        indexText English par doc
        addDocument db doc
        

main =
 do args <- getArgs
    progName <- getProgName
    case args of
         path:_ -> do dat <- getContents 
                      indexParagraphs path (paragraphs dat)
         _      -> putStrLn $ "Usage: " ++ progName ++ " PATH_TO_DATABASE"
