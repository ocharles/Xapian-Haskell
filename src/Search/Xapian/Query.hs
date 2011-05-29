{-# LANGUAGE TypeSynonymInstances #-}

module Search.Xapian.Query
     ( -- * Generic Query constructor
       Queryable (..)

       -- * Convenience functions
       , rawQuery
       , resultsFromTo
       , paging
       
       -- * Lower level
       , compileQuery
       , describeQuery
     ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack, useAsCString, ByteString)


import Search.Xapian.Types
import Search.Xapian.Internal.Types
import Search.Xapian.Internal.Utils
import Search.Xapian.Internal.FFI


-- * using queries with strings, utf-16, whatever
class Queryable s where
    -- | @query term@ will create a verbatim Query object for @term@.
    query :: s -> Query

instance Queryable String where
    query = Atom . pack

instance Queryable ByteString where
    query = Atom

-- * Convenience functions

-- | FIXME: I'm unreliable
-- | @rawQuery stemmer query@ will store a natural language @query@ into an
-- aggregate "Query" value. The stemming algorithm @stem@ will be used on
-- query terms.
--
rawQuery :: Queryable s
         => Stemmer -- ^ The stemming algorithm to apply to query terms
         -> s       -- ^ The plain text query to parse
         -> Query   -- ^ The aggregate query
rawQuery stemmer queryString =
    let Atom bs = query queryString
    in  Parsed stemmer bs

resultsFromTo :: Int -> Int -> QueryRange
resultsFromTo from to = QueryRange from (from - to + 1)

paging :: Int -> Int -> QueryRange
paging page amount = QueryRange (amount * page) amount

-- * Low-level stuff

class GetOpCode t where
    opcode :: t -> CInt

-- TODO: write proper bindings to access opcodes, bugs incoming
instance GetOpCode OpNullary where
    opcode (OpValueGE _ _)      = cx_query_OP_VALUE_GE
    opcode (OpValueLE _ _)      = cx_query_OP_VALUE_LE
    opcode (OpValueRange _ _ _) = cx_query_OP_VALUE_RANGE

instance GetOpCode OpUnary where
    opcode (OpScaleWeight _) = 9

instance GetOpCode OpBinary where
    opcode OpOr       = cx_query_OP_OR
    opcode OpAnd      = cx_query_OP_AND
    opcode OpXor      = cx_query_OP_XOR
    opcode OpAndMaybe = cx_query_OP_AND_MAYBE
    opcode OpAndNot   = cx_query_OP_AND_NOT
    opcode OpFilter   = cx_query_OP_FILTER
    opcode OpEliteSet = cx_query_OP_ELITE_SET

instance GetOpCode OpMulti where
    opcode OpSynonym = cx_query_OP_SYNONYM

instance GetOpCode OpMultiFlat where
    opcode (OpPhrase _) = cx_query_OP_PHRASE
    opcode (OpNear _)   = cx_query_OP_NEAR

compileQuery :: ReadOnlyDB -> Query -> IO (ForeignPtr CQuery)
compileQuery db@(ReadOnlyDB dbmptr) query' =
    case query' of
         MatchAll   -> cx_query_match_all >>= manage
         MatchNothing -> cx_query_match_nothing >>= manage
         Atom bs    -> useAsCString bs $ \cs ->
                       cx_query_new_0 cs 1 0 >>= manage
         Parsed stemmer bs
                    -> (createStemmer stemmer >>=) $
                       (flip withForeignPtr) $ \stemPtr ->
                       withForeignPtr dbmptr $ \dbptr ->
                        do qp <- cx_queryparser_new
                           useAsCCString bs $ \ccs ->
                            do cx_queryparser_set_database qp dbptr
                               cx_queryparser_set_stemmer  qp stemPtr
                               cx_queryparser_set_stemming_strategy qp
                                   cx_queryparser_STEM_SOME
                               cx_queryparser_parse_query_simple qp ccs
                                   >>= manage
         Nullary op -> compileNullary op
         Unary op q -> do cq <- compileQuery db q
                          compileUnary op cq
         Binary op q q' -> do cq <- compileQuery db q
                              cq' <- compileQuery db q'
                              compileBinary op cq cq'
         Multi op qs    -> do cqs <- mapM (compileQuery db) qs
                              compileMulti op cqs
         MultiFlat op qs -> do cqs <- mapM (compileQuery db . query) qs
                               compileMultiFlat op cqs
  where
    compileNullary op@(OpValueGE valno val) =
        useAsCString val $ \cs ->
        cx_query_new_6 (opcode op) valno cs >>= manage
    compileNullary op@(OpValueLE valno val) =
        useAsCString val $ \cs ->
        cx_query_new_6 (opcode op) valno cs >>= manage
    compileNullary op@(OpValueRange valno lower upper) =
        useAsCCString lower $ \cclower ->
        useAsCCString upper $ \ccupper ->
        cx_query_new_5 (opcode op) (fromIntegral valno)
            cclower ccupper >>= manage

    compileUnary op@(OpScaleWeight s) q =
        withForeignPtr q $ \cq ->
        cx_query_new_4 (opcode op) cq s >>= manage

    compileBinary op q q' = merge (opcode op) q q'

    compileMulti op@(OpSynonym) qs =
     do mvec <- manage =<< cx_vector_new
        withForeignPtr mvec $ \vec ->
         do forM_ qs $ \q -> withForeignPtr q $ cx_vector_append vec
            mb <- newForeignPtr_ =<< cx_vector_begin vec
            me <- newForeignPtr_ =<< cx_vector_end   vec
            withForeignPtr mb $ \b -> withForeignPtr me $ \e ->
                cx_query_new_3 (opcode op) b e 0 >>= manage


    compileMultiFlat op@(OpPhrase windowSize) qs =
     do mvec <- manage =<< cx_vector_new
        withForeignPtr mvec $ \vec ->
         do forM_ qs $ \q -> withForeignPtr q $ cx_vector_append vec
            mb <- newForeignPtr_ =<< cx_vector_begin vec
            me <- newForeignPtr_ =<< cx_vector_end   vec
            withForeignPtr mb $ \b -> withForeignPtr me $ \e ->
                cx_query_new_3 (opcode op) b e (fromIntegral windowSize)
                >>= manage

    compileMultiFlat op@(OpNear distance) qs =
     do mvec <- manage =<< cx_vector_new
        withForeignPtr mvec $ \vec ->
         do forM_ qs $ \q -> withForeignPtr q $ cx_vector_append vec
            mb <- newForeignPtr_ =<< cx_vector_begin vec
            me <- newForeignPtr_ =<< cx_vector_end   vec
            withForeignPtr mb $ \b -> withForeignPtr me $ \e ->
                cx_query_new_3 (opcode op) b e (fromIntegral distance)
                >>= manage

    merge :: CInt
          -> ForeignPtr CQuery
          -> ForeignPtr CQuery
          -> IO (ForeignPtr CQuery)
    merge opcode_ a b =
        withForeignPtr a $ \queryA ->
        withForeignPtr b $ \queryB ->
            cx_query_new_1 opcode_ queryA queryB >>= manage

-- * Helper functions


-- | @describeQuery query@ will result in a plain text description
-- of @query@. Note, this is not really meant for human consumption, but
-- may help for debugging.
--
describeQuery :: QueryPtr -> IO String
describeQuery q =
  withForeignPtr q $ \query' ->
  peekCString =<< cx_query_get_description query'
