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

       , StemmerPtr
       , Stemmer (..)
       ) where

import Foreign
import Foreign.C.Types
import Data.ByteString.Char8 (ByteString)

import Search.Xapian.Internal.FFI

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

data Database fields dat = Database !(ForeignPtr CDatabase)
  deriving (Eq, Show)

newtype WritableDatabase fields dat = WritableDatabase (Database fields dat)


-- * Query related types
-- --------------------------------------------------------------------

type QueryPtr = ForeignPtr CEnquire

-- Internal Representation of Queries

data Query
    = MatchNothing        -- ^ does not match anything
    | MatchAll            -- ^ matches everything
    | Atom ByteString
    | Parsed  {-# UNPACK #-} Stemmer ByteString -- ^ parsed natively by Xapian
    | Nullary {-# UNPACK #-} OpNullary
    | Unary   {-# UNPACK #-} OpUnary   Query
    | Binary  {-# UNPACK #-} OpBinary  Query  Query
    | Multi   {-# UNPACK #-} OpMulti  [Query]
    deriving (Show)

data OpNullary
    = OpValueGE {-# UNPACK #-} ValueNumber Value
    | OpValueLE {-# UNPACK #-} ValueNumber Value
    | OpValueRange {-# UNPACK #-} ValueNumber [Value]
    deriving (Show)

data OpUnary
    = OpScaleWeight {-# UNPACK #-} Double -- Xapian::InvalidArgumentError if scale is negative
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
    | OpPhrase {-# UNPACK #-} Int
    deriving (Show)


-- * Document related types
-- --------------------------------------------------------------------

type DocumentPtr = ForeignPtr CDocument

-- * document fields
type ValueNumber = CUInt
type Value       = ByteString

-- * Stemming related types
-- --------------------------------------------------------------------

type StemmerPtr = ForeignPtr CStem

data Stemmer = Danish
             | Dutch
             | DutchKraaijPohlmann -- ^ A different Dutch stemmer
             | English       -- ^ Martin Porter's 2002 revision of his stemmer
             | EnglishLovins -- ^ Lovin's stemmer
             | EnglishPorter -- ^ Porter's stemmer as described in his 1980 paper
             | Finnish
             | French
             | German
             | German2 -- ^ Normalises umlauts and ß
             | Hungarian
             | Italian
             | Norwegian
             | Portuguese
             | Romanian
             | Russian
             | Spanish
             | Swedish
             | Turkish
             deriving (Show, Eq)
