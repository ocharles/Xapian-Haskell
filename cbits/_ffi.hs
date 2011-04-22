{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

import Foreign
import Foreign.C.String
import Foreign.C.Types

import qualified Data.ByteString as BS

-- Query
-- ---------------------------------------------------------

data CQuery
type Op = Int

foreign import ccall "OP_AND" cx_query_OP_AND :: Int
foreign import ccall "OP_OR" cx_query_OP_OR :: Int
foreign import ccall "OP_AND_NOT" cx_query_OP_AND_NOT :: Int
foreign import ccall "OP_XOR" cx_query_OP_XOR :: Int
foreign import ccall "OP_AND_MAYBE" cx_query_OP_AND_MAYBE :: Int
foreign import ccall "OP_FILTER" cx_query_OP_FILTER :: Int
foreign import ccall "OP_NEAR" cx_query_OP_NEAR :: Int
foreign import ccall "OP_PHRASE" cx_query_OP_PHRASE :: Int
foreign import ccall "OP_VALUE_RANGE" cx_query_OP_VALUE_RANGE :: Int
foreign import ccall "OP_SCALE_WEIGHT" cx_query_OP_SCALE_WEIGHT :: Int
foreign import ccall "OP_ELITE_SET" cx_query_OP_ELITE_SET :: Int
foreign import ccall "OP_VALUE_GE" cx_query_OP_VALUE_GE :: Int
foreign import ccall "OP_VALUE_LE" cx_query_OP_VALUE_LE :: Int
foreign import ccall "OP_SYNONYM" cx_query_OP_SYNONYM :: Int

foreign import ccall "query_new"
    cx_query_new :: IO (Ptr CQuery)

foreign import ccall "query_copy"
    cx_query_copy :: Ptr CQuery -> IO (Ptr CQuery)
  
foreign import ccall "query_delete"
    cx_query_delete :: Ptr CQuery -> IO ()

foreign import ccall "query_new_0"
    cx_query_new_0 :: CString -> CUInt -> CUInt -> IO (Ptr CQuery)

foreign import ccall "query_new_1"
    cx_query_new_1 :: Op -> Ptr CQuery -> Ptr CQuery -> IO (Ptr CQuery)

foreign import ccall "query_new_2"
    cx_query_new_2 :: Op -> CString -> CString -> IO (Ptr CQuery)

--foreign import ccall "query_new_3"
--    cx_query_new_3 :: Op -> CString -> CString -> IO (Ptr CQuery)

foreign import ccall "query_new_4"
    cx_query_new_4 :: Op -> Ptr CQuery -> CDouble -> IO (Ptr CQuery)

--foreign import ccall "query_new_5"
--    cx_query_new_5 :: Op -> CString -> CString -> IO (Ptr CQuery)

foreign import ccall "query_new_6"
    cx_query_new_6 :: Op -> CUInt -> CString -> IO (Ptr CQuery)

-- foreign import ccall "query_new_7"

-- assume purity
foreign import ccall "query_match_all"
    cx_query_match_all :: Ptr CQuery

-- assume purity
foreign import ccall "query_match_nothing"
    cx_query_match_nothing :: Ptr CQuery

-- assume purity
foreign import ccall "query_get_length"
    cx_query_get_length :: Ptr CQuery -> CUInt

-- assume purity
foreign import ccall "query_empty"
    cx_query_empty :: Ptr CQuery -> Bool
 
-- assume purity
foreign import ccall "query_serialise"
    cx_query_serialise :: Ptr CQuery -> CString

-- assume purity
foreign import ccall "query_get_terms_begin"
    cx_query_get_terms_begin :: Ptr CQuery -> Ptr CTermIterator

-- assume purity
foreign import ccall "query_get_terms_end"
    cx_query_get_terms_end :: Ptr CQuery -> Ptr CTermIterator

-- assume purity
foreign import ccall "query_get_description"
    cx_query_get_description :: Ptr CQuery -> CString


-- PositionIterator
-- ---------------------------------------------------------

data CPositionIterator

foreign import ccall "positioniterator_new"
    cx_positioniterator_new :: IO (Ptr CPositionIterator)

foreign import ccall "positioniterator_copy"
    cx_positioniterator_copy :: Ptr CPositionIterator
                             -> IO (Ptr CPositionIterator)

foreign import ccall "positioniterator_delete"
    cx_positioniterator_delete :: Ptr CPositionIterator -> IO ()

foreign import ccall "positioniterator_skip_to"
    cx_positioniterator_skip_to :: Ptr CPositionIterator -> CUInt -> IO ()

-- assume purity
foreign import ccall "positioniterator_get_description"
    cx_positioniterator_get_description :: Ptr CPositionIterator
                                        -> CString

-- TermIterator
-- ---------------------------------------------------------

data CTermIterator

foreign import ccall "termiterator_new"
    cx_termiterator_new :: IO (Ptr CTermIterator)

foreign import ccall "termiterator_copy"
    cx_termiterator_copy :: Ptr CTermIterator -> IO (Ptr CTermIterator)

foreign import ccall "termiterator_delete"
    cx_termiterator_delete :: Ptr CTermIterator -> IO ()

foreign import ccall "termiterator_next"
    cx_termiterator_next :: Ptr CTermIterator -> IO ()

-- assume purity
foreign import ccall "termiterator_is_end"
    cx_termiterator_is_end :: Ptr CTermIterator -> Ptr CTermIterator
                           -> Bool

-- assume purity
foreign import ccall "termiterator_get"
    cx_termiterator_get :: Ptr CTermIterator -> CString

foreign import ccall "termiterator_skip_to"
    cx_termiterator_skip_to :: Ptr CTermIterator -> CString -> IO ()

-- assume purity
foreign import ccall "termiterator_get_wdf"
    cx_termiterator_get_wdf :: Ptr CTermIterator -> CUInt

-- assume purity
foreign import ccall "termiterator_get_termfreq"
    cx_termiterator_get_termfreq :: Ptr CTermIterator -> CUInt

-- assume purity
foreign import ccall "termiterator_positionlist_count"
    cx_termiterator_positionlist_count :: Ptr CTermIterator -> CUInt

-- assume purity
foreign import ccall "termiterator_positionlist_begin"
    cx_termiterator_positionlist_begin :: Ptr CTermIterator
                                       -> Ptr CPositionIterator

-- assume purity
foreign import ccall "termiterator_positionlist_end"
    cx_termiterator_positionlist_end :: Ptr CTermIterator
                                     -> Ptr CPositionIterator

collect :: Ptr a -- current position
        -> Ptr a -- end
        -> (Ptr a -> IO ()) -- next
        -> (Ptr a -> IO b)  -- get
        -> (Ptr a -> Ptr a -> Bool) -- finished?
        -> IO [b]
collect pos end next get finished =
 if finished pos end
    then do return []
    else do elem <- get pos
            next pos
            rest <- collect pos end next get finished
            return (elem : rest)

test =
  do p <- newCString "Protoss"
     t <- newCString "Terran"
     z <- newCString "Zerg"

     q1 <- cx_query_new_2 cx_query_OP_OR p t
     q2 <- cx_query_new_0 z 1 0
     q  <- cx_query_new_1 cx_query_OP_OR q1 q2

     peekCString (cx_query_get_description q) >>= putStrLn

     let b = cx_query_get_terms_begin q
         e = cx_query_get_terms_end   q

     all <- collect b e cx_termiterator_next
                (peekCString . cx_termiterator_get)
                cx_termiterator_is_end
     print all
     return all
