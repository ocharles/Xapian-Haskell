-- | this module is not intended to be made visible
module Search.Xapian.Internal.Types
       ( Error (..)
       , NativeError (..)

       , WritableDatabase (..)
       , Database (..)

       , Query (..)
       , OpNullary (..)
       , OpUnary (..)
       , OpBinary (..)
       , OpMulti (..)
       , QueryPtr

       , DocumentPtr
       , ValueNumber
       , Value
       ) where

import Foreign
import Data.ByteString.Char8 (ByteString)

import Search.Xapian.FFI

-- * Error types
-- --------------------------------------------------------------------

-- | Error, inspired (blatantly copied) by hdbc
--
-- The main Xapian exception object. As much information as possible is passed from
-- the database through to the application through this object.
--
-- Errors generated in the Haskell layer will have seNativeError set to
-- Nothing.
-- 



data Error = Error { xeNativeError :: Maybe NativeError
                   , seErrorMsg :: String
                   } deriving (Show)

data NativeError = DocNotFoundError | GenericError
  deriving (Eq, Show)


-- * Database related types
-- --------------------------------------------------------------------

data Database fields dat = Database !(ForeignPtr XapianDatabase)
  deriving (Eq, Show)

newtype WritableDatabase fields dat = WritableDatabase (Database fields dat)


-- * Query related types
-- --------------------------------------------------------------------

type QueryPtr = ForeignPtr XapianEnquire

-- Internal Representation of Queries

data Query
    = EmptyQuery -- ^ does not match anything
    | Atom ByteString
    | Nullary OpNullary
    | Unary  OpUnary   Query
    | Binary OpBinary  Query  Query
    | Multi  OpMulti  [Query]
    deriving (Show)

data OpNullary
    = OpValueGE ValueNumber Value
    | OpValueLE ValueNumber Value
    | OpValueRange ValueNumber [Value]
    deriving (Show)

data OpUnary
    = OpScaleWeight Double -- Xapian::InvalidArgumentError if scale is negative
    deriving (Show)

data OpBinary
    = OpOr 
    | OpEliteSet
    | OpAnd 
    | OpXor 
    | OpAndMaybe 
    | OpAndNot 
    | OpFilter 
    | OpNear Int
    deriving (Show)

data OpMulti
    = OpSynonym
    | OpPhrase Int
    deriving (Show)


-- * Document related types
-- --------------------------------------------------------------------

type DocumentPtr = ForeignPtr XapianDocument

-- * document fields
type ValueNumber = Int
type Value       = ByteString

