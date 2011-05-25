{-# LANGUAGE TypeSynonymInstances #-}

module Search.Xapian.Query
     ( -- * Generic Query constructor
       Queryable (..)

       -- * Convenience functions
       , rawQuery
       , queryAll
       , queryAny
       , resultsFromTo
       , paging
       
       -- * Lower level
       , compileQuery
     ) where

import Foreign
import Foreign.C.String
import Data.ByteString.Char8 (pack, useAsCString, ByteString)


import Search.Xapian.Types
import Search.Xapian.Internal.Types
import Search.Xapian.Internal.Utils
import Search.Xapian.Internal.FFI
import Search.Xapian.Query.Combinators as Q


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

queryAll :: Queryable s =>  [s] -> Query
queryAll [] = MatchAll
queryAll xs = foldr1 Q.and $ map query xs

queryAny :: Queryable s => [s] -> Query
queryAny [] = MatchNothing
queryAny xs = foldr1 Q.eliteSet $ map query xs

-- * Low-level stuff

class GetOpCode t where
    opcode :: t -> Int

-- TODO: write proper bindings to access opcodes, bugs incoming
instance GetOpCode OpNullary where
    opcode (OpValueGE _ _)    = cx_query_OP_VALUE_GE
    opcode (OpValueLE _ _)    = cx_query_OP_VALUE_LE
    opcode (OpValueRange _ _) = cx_query_OP_VALUE_RANGE

instance GetOpCode OpUnary where
    opcode (OpScaleWeight _) = 9

instance GetOpCode OpBinary where
    opcode OpOr       = cx_query_OP_OR
    opcode OpAnd      = cx_query_OP_AND
    opcode OpXor      = cx_query_OP_XOR
    opcode OpAndMaybe = cx_query_OP_AND_MAYBE
    opcode OpAndNot   = cx_query_OP_AND_NOT
    opcode OpFilter   = cx_query_OP_FILTER
    opcode (OpNear _) = cx_query_OP_NEAR
    opcode OpEliteSet = cx_query_OP_ELITE_SET

instance GetOpCode OpMulti where
    opcode OpSynonym    = cx_query_OP_SYNONYM
    opcode (OpPhrase _) = cx_query_OP_PHRASE

compileQuery :: Query -> IO (ForeignPtr CQuery)
compileQuery query' =
    case query' of
         MatchAll   -> cx_query_match_all >>= manage
         MatchNothing -> cx_query_match_nothing >>= manage
         Atom bs    -> useAsCString bs $ \cs ->
                       cx_query_new_0 cs 1 0 >>= manage
         Parsed stemmer bs
                    -> useAsCString bs $ \cs ->
                       (createStemmer stemmer >>=) $
                       (flip withForeignPtr) $ \stemPtr ->
                       -- FIXME: write FFI for Xapian::QueryParser
                       error "cx_parse_query cs stemPtr >>= manage"
         Nullary op -> compileNullary op
         Unary op q -> do cq <- compileQuery q
                          compileUnary op cq
         Binary op q q' -> do cq <- compileQuery q
                              cq' <- compileQuery q'
                              compileBinary op cq cq'
         Multi op qs    -> do cqs <- mapM compileQuery qs
                              compileMulti op cqs
  where
    compileNullary op@(OpValueGE valno val) =
        useAsCString val $ \cs ->
        cx_query_new_6 (opcode op) valno cs >>= manage
    compileNullary op@(OpValueLE valno val) =
        useAsCString val $ \cs ->
        cx_query_new_6 (opcode op) valno cs >>= manage
    compileNullary op@(OpValueRange valno vals) =
        undefined -- TODO

    compileUnary op@(OpScaleWeight s) q =
        withForeignPtr q $ \cq ->
        cx_query_new_4 (opcode op) cq s >>= manage

    compileBinary (OpNear distance) q q' =
     do undefined -- TODO
    compileBinary op q q' = merge (opcode op) q q'

    compileMulti op@(OpSynonym) qs =
        undefined -- TODO
    compileMulti op@(OpPhrase windowSize) qs =
        undefined -- TODO

    merge :: Int
          -> ForeignPtr CQuery
          -> ForeignPtr CQuery
          -> IO (ForeignPtr CQuery)
    merge opcode a b =
        withForeignPtr a $ \queryA ->
        withForeignPtr b $ \queryB ->
            cx_query_new_1 opcode queryA queryB >>= manage

-- * Helper functions


-- | @describeQuery query@ will result in a plain text description
-- of @query@. Note, this is not really meant for human consumption, but
-- may help for debugging.
--
describeQuery :: QueryPtr -> IO String
describeQuery q =
  withForeignPtr q $ \query' ->
  peekCString =<< cx_query_get_description query'
