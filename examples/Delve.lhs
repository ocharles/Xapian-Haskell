> {-# LANGUAGE TupleSections #-}
> import Data.Enumerator (($$))
> import Control.Applicative
> import Control.Monad (when)
> import qualified Data.Enumerator.List as EL
> import qualified Data.Enumerator as E
> import System (getArgs, getProgName)
> import System.Console.GetOpt
> import Search.Xapian
> import qualified Data.ByteString.Char8 as BS
> import Data.ByteString (ByteString)
> import Control.Monad.Trans (liftIO)

> main =
>  do Right db <- openReadOnly "simple.db"
>     let printer = printTerm False
>     printAllTerms db printer
>     mapM_ (printDocData db) [1,2,3]
>     mapM_ (printTermList db printer) [1,2,3]
>     printUUID db
>     printDocCount db
>     printPostingList db (BS.pack "circles")


> printAllTerms db printer =
>  do putStr "All terms in database:"
>     let feedAllTerms = getAllTerms db 1024
>     runXapian $ E.run_ $ feedAllTerms $$ EL.foldM (\_ term ->
>         printer term) ()
>     putStrLn ""

> printPostingList db term =
>  do putStr "Posting List for term `" >> BS.putStr term >> putStr "' "
>     (termfreq, collfreq, wdf_max) <- runXapian $
>         (,,) <$> termFreq db term <*> collFreq db term <*> wdfMax db term
>     putStr $ concat ["(termfreq ", show termfreq
>                     , ", collfreq ", show collfreq
>                     , ", wdf_max ", show wdf_max, "):"]
>     let feedPostings = getPostings db term 1024
>     runXapian $ E.run_ $ feedPostings $$ EL.foldM (\_ (docid,_)  ->
>         liftIO $ putStr " " >> putStr (show $ getDocId docid)) ()
>     putStrLn ""

> printUUID db =
>  do uuid <- runXapian $ getUUID db
>     putStr "UUID = "
>     BS.putStrLn uuid

> printDocCount db =
>  do docCount <- runXapian $ getDocCount db
>     putStrLn $ "number of documents = " ++ show docCount

> printTermList db printer docid =
>  do putStr $ "Term List for record #" ++ show docid ++ ":"
>     runXapian $
>      do edoc <- getDocument db (DocId $ fromIntegral docid)
>         case edoc of
>         { Right doc -> getTerms doc >>= mapM_ printer . map unTerm
>         ; Left err  -> liftIO $ putStrLn (show err)
>         }
>     putStrLn ""
>     where
>     unTerm (Term bs _) = bs

> printDocData db docid =
>  do putStrLn $ "Data for record #" ++ show docid ++ ":"
>     runXapian $
>      do edoc <- getDocument db (DocId $ fromIntegral docid)
>         case edoc of
>         { Right doc -> getData doc >>= liftIO . BS.putStrLn
>         ; Left err  -> liftIO $ putStrLn (show err)
>         }

> printTerm :: Bool -> ByteString -> XapianM ()
> printTerm one_per_line term = liftIO $
>  do when      one_per_line  $ putStrLn ""
>     when (not one_per_line) $ putStr  " "
>     BS.putStr term
