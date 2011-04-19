{-# LANGUAGE TypeSynonymInstances #-}

module Search.Xapian.Query
     ( -- * Generic Query constructor
       Queryable (..)

       -- * Convenience functions
       , queryAll
       , queryAny
       , resultsFromTo
       , paging
       
       -- * Lower level
       , compileQuery
     ) where

import Foreign
import Foreign.C.String
import Control.Monad (forM_)
import Data.ByteString.Char8 (pack, useAsCString, ByteString)


import Search.Xapian.Types
import Search.Xapian.Internal.Types
import Search.Xapian.FFI
import Search.Xapian.Query.Combinators as Q


-- * using queries with strings, utf-16, whatever

class Queryable s where
    query :: s -> Query

instance Queryable String where
    query = Atom . pack

instance Queryable ByteString where
    query = Atom

-- * Convenience functions

resultsFromTo :: Int -> Int -> QueryRange
resultsFromTo from to = QueryRange from (from - to + 1)

paging :: Int -> Int -> QueryRange
paging page amount = QueryRange (amount * page) amount

queryAll :: Queryable s =>  [s] -> Query
queryAll [] = EmptyQuery
queryAll xs = foldr1 Q.and $ map query xs

queryAny :: Queryable s => [s] -> Query
queryAny [] = EmptyQuery
queryAny xs = foldr1 Q.eliteSet $ map query xs

-- * Low-level stuff

class GetOpCode t where
    opcode :: t -> Int

-- TODO: write proper bindings to access opcodes, bugs incoming
instance GetOpCode OpNullary where
    opcode (OpValueGE _ _) = 11
    opcode (OpValueLE _ _) = 12
    opcode (OpValueRange _ _) = 8

instance GetOpCode OpUnary where
    opcode (OpScaleWeight _) = 9

instance GetOpCode OpBinary where
    opcode OpOr = 1
    opcode OpAnd = 0
    opcode OpXor = 3
    opcode OpAndMaybe = 4
    opcode OpAndNot = 2
    opcode OpFilter = 5
    opcode (OpNear _) = 6
    opcode OpEliteSet = 10

instance GetOpCode OpMulti where
    opcode OpSynonym = 13
    opcode (OpPhrase _) = 7

data QueryIterator

compileQuery :: Query -> IO QueryPtr
compileQuery query =
    case query of
         EmptyQuery -> c_xapian_query_empty >>= manage
         Atom bs    -> useAsCString bs $ \cs ->
                       c_xapian_query_new cs >>= manage
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
        c_xapian_query_new_value (opcode op) valno cs >>= manage
    compileNullary op@(OpValueLE valno val) =
        useAsCString val $ \cs ->
        c_xapian_query_new_value (opcode op) valno cs >>= manage
    compileNullary op@(OpValueRange valno vals) =
        undefined -- TODO

    compileUnary op@(OpScaleWeight s) q =
        withForeignPtr q $ \cq ->
        c_xapian_query_new_double (opcode op) cq s >>= manage

    compileBinary (OpNear distance) q q' =
     do undefined -- TODO
    compileBinary op q q' = merge (opcode op) q q'

    compileMulti op@(OpSynonym) qs =
        undefined -- TODO
    compileMulti op@(OpPhrase windowSize) qs =
        undefined -- TODO

    merge :: Int -> QueryPtr -> QueryPtr -> IO QueryPtr 
    merge opcode a b =
        withForeignPtr a $ \queryA ->
        withForeignPtr b $ \queryB ->
            c_xapian_query_combine opcode queryA queryB >>= manage

-- * Helper functions

manage :: Ptr XapianQuery -> IO QueryPtr
manage = newForeignPtr c_xapian_query_delete

describeQuery :: QueryPtr -> IO String
describeQuery q =
  withForeignPtr q $ \query' ->
  peekCString $ c_xapian_query_describe query'
