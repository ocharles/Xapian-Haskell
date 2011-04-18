{-# LANGUAGE TypeSynonymInstances #-}

module Search.Xapian.Query
     ( -- * Generic Query constructor
       Queryable (..)
       
       -- * Lower level
       , QueryPtr
       , compileQuery
     ) where

import Foreign
import Foreign.C.String
import Search.Xapian.Types
import Search.Xapian.Internal.Types
import Search.Xapian.FFI

import Data.ByteString.Char8 (pack, ByteString)


-- | Most likely you want to import this module as a qualified module, i.e.
-- 
-- > import qualified Search.Xapian.Query as Q


-- * using queries with strings, utf-16, whatever

class Queryable s where
  query :: s -> Query

instance Queryable String where
  query = Atom . pack

instance Queryable ByteString where
  query = Atom

-- * Combinators

--
valueGreaterEqual :: ValueNumber -> Value -> Query
valueGreaterEqual valno val = Nullary $ OpValueGE valno val

valueLowerEqual :: ValueNumber -> Value -> Query
valueLowerEqual   valno val = Nullary $ OpValueLE valno val

valueInRange :: ValueNumber -> [Value] -> Query
valueInRange valno vals = Nullary $ OpValueRange valno vals

--
not :: Query -> Query
not = Unary OpNot

scale :: Double -> Query -> Query
scale s = Unary (OpScaleWeight s)

eliteSet :: Query -> Query
eliteSet = Unary OpEliteSet

--
or :: Query -> Query -> Query
or = Binary OpOr

and :: Query -> Query -> Query
and = Binary OpAnd

xor :: Query -> Query -> Query
xor = Binary OpXor

andMaybe :: Query -> Query -> Query
andMaybe = Binary OpAndMaybe

andNot :: Query -> Query -> Query
andNot = Binary OpAndNot

filter :: Query -> Query -> Query
filter = Binary OpFilter

near :: Int -> Query -> Query -> Query
near distance = Binary (OpNear distance)

--
synonyms :: [Query] -> Query
synonyms = Multi OpSynonym

phrase :: Int -> [Query] -> Query
phrase windowSize = Multi (OpPhrase windowSize)

-- * C-level stuff

compileQuery :: Query -> IO QueryPtr
compileQuery =
    undefined
  where
    combineQueries a b operator =
        withForeignPtr a $ \queryA ->
        withForeignPtr b $ \queryB ->
         do query <- c_xapian_query_combine operator queryA queryB
            newForeignPtr c_xapian_query_delete query




-- * Helper functions

describeQuery :: ForeignPtr XapianQuery -> IO String
describeQuery q =
  withForeignPtr q $ \query' ->
  peekCString $ c_xapian_query_describe query'
