> import System.Console.GetOpt
> import System (getArgs)
> import System.IO (hFlush, stdout)
> import Control.Monad (forM_)
> import Control.Monad.Trans (liftIO)
> import Data.ByteString.Char8 (pack)
> --import Control.Concurrent (threadDelay)

> import Search.Xapian

> data CmdlineArgs
>     = NoRenumber
>     | Help
>     | Version
>     deriving (Show, Eq)

> cmdlineArgs =
>     [ Option "" ["no-renumber"] (NoArg NoRenumber) (unlines $ 
>       ["Preserve the numbering of document ids (useful if you have"
>       ,"external references to them, or have set them to match"
>       ,"unique ids from an external source).  If multiple source"
>       ,"databases are specified and the same docid occurs in more"
>       ,"one, the last occurrence will be the one which ends up in"
>       ,"the destination database."])
>     , Option "h" ["help"] (NoArg Help) "display this help and exit"
>     , Option "v" ["version"] (NoArg Version) "output version information and exit"
>     ]

> handleArgs :: [CmdlineArgs] -> IO () -> IO ()
> handleArgs (Help:_)     _ = putStrLn (usageInfo "CopyDatabase\n" cmdlineArgs)
> handleArgs (Version:_)  _ = putStrLn "pre-alpha"
> handleArgs (_:rest)  prog = handleArgs rest prog
> handleArgs [] prog        = prog

> main =
>  do args <- getArgs
>     case getOpt Permute cmdlineArgs args of
>          (_ , _, (x:xs))    -> mapM_ putStrLn (x:xs)
>          (xs, dbs@(_:_:_), _) -> handleArgs xs $
>           do let renumber = not $ NoRenumber `elem` xs
>                  dst = last dbs
>                  srcs = init dbs
>              dstDB <- failing =<< openReadWrite Create dst
>              forM_ srcs $ \src ->
>               do srcDB <- failing =<< openReadOnly src
>                  runXapian $ copyDocuments renumber src srcDB dstDB
>                  putStrLn ""
>                  wrap "spelling data" $ copySpellingData srcDB dstDB
>                  wrap "synonym data"  $ copySynonymData srcDB dstDB
>                  wrap "user metadata" $ copyMetadata srcDB dstDB
>          _ -> handleArgs [Help] (return ())

>     where

>     failing (Left err) = error (show err)
>     failing (Right a)  = return a

>     copyDocuments renumber src srcDB dstDB =
>      do postings <- getPostings srcDB (pack "")
>         doccount <- getDocCount srcDB
>
>         let showStatus i = liftIO $
>              do putStr $ "\r" ++ src ++ " " ++ show i
>                               ++ " of " ++ show doccount
>                 hFlush stdout
>                 --threadDelay 10000
>
>         forM_ (zip [1..] postings) $ \(i, (docid,_)) ->
>          do edoc <- getDocument srcDB docid
>             case edoc of
>                  Right doc ->
>                     if renumber
>                        then do addDocument dstDB doc
>                                showStatus i
>                        else do replaceDocument dstDB docid doc
>                                showStatus i
>                  Left  err -> liftIO $ putStrLn (show err)

>     wrap target xapianAction =
>      do putStr $ "Copying " ++ target ++ "..."
>         runXapian xapianAction
>         putStrLn " done."     

>     copySpellingData srcDB dstDB =
>      do spellings <- getSpellings srcDB
>         forM_ spellings $ uncurry (addSpelling dstDB)

>     copySynonymData srcDB dstDB =
>      do terms <- getSynonymKeys srcDB (pack "")
>         forM_ terms $ \term ->
>          do synonyms <- getSynonyms srcDB term
>             forM_ synonyms $ \synonym ->
>                 addSynonym dstDB term synonym

>     copyMetadata srcDB dstDB =
>      do keys <- getMetadataKeys srcDB (pack "")
>         forM_ keys $ \key ->
>          do value <- getMetadata srcDB key
>             setMetadata dstDB key value
