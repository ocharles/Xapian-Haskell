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
       , Document (..)
       , SimpleDocument
       , DocumentDiff (..)
       , Prefixable (..)
       , Fieldless
       , DocumentId (..)
       , Term (..)
       , Pos

       , StemPtr
       , Stemmer (..)
       ) where

import Foreign
import Foreign.C.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Sequence (Seq)

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

data NativeError
    = DatabaseOpeningError
    | DocNotFoundError
    | GenericError
  deriving (Eq, Show)


-- * Database related types
-- --------------------------------------------------------------------

data Database fields dat = Database !(ForeignPtr CDatabase)
  deriving (Eq, Show)

newtype WritableDatabase fields dat = WritableDatabase (Database fields dat)


-- * Query related types
-- --------------------------------------------------------------------

-- Internal Representation of Queries

data Query
    = MatchNothing        -- ^ does not match anything
    | MatchAll            -- ^ matches everything
    | Atom ByteString
    | Parsed  Stemmer ByteString -- ^ parsed natively by Xapian
    | Nullary OpNullary
    | Unary   OpUnary   Query
    | Binary  OpBinary  Query  Query
    | Multi   OpMulti  [Query]
    deriving (Show)

data OpNullary
    = OpValueGE {-# UNPACK #-} !ValueNumber Value
    | OpValueLE {-# UNPACK #-} !ValueNumber Value
    | OpValueRange {-# UNPACK #-} !ValueNumber [Value]
    deriving (Show)

data OpUnary
    = OpScaleWeight {-# UNPACK #-} !Double -- Xapian::InvalidArgumentError if scale is negative
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
    | OpPhrase {-# UNPACK #-} !Int
    deriving (Show)


-- * Document related types
-- --------------------------------------------------------------------

-- * document fields
type ValueNumber = CUInt
type Value       = ByteString

class Ord fields => Prefixable fields where
    getPrefix   :: fields -> ByteString
    stripPrefix :: fields -> ByteString -> Maybe ByteString

data Fieldless = Fieldless deriving (Show, Ord, Eq)

instance Prefixable Fieldless where
    getPrefix   Fieldless = BS.empty
    stripPrefix Fieldless = const Nothing

type SimpleDocument = Document Fieldless

-- | doc_id == 0 is invalid; what is the range of
newtype DocumentId = DocId { getDocId :: Word32 }
    deriving (Show, Eq)

-- | A @Document@
data Document fields dat = Document
    { documentPtr   :: Maybe DocumentPtr            
    , documentId    :: Maybe DocumentId       
    , documentLazyStem  :: Maybe Stemmer      -- ^ the stemmer is being committed as well
    , documentLazyValues :: IntMap Value          
    , documentLazyTerms :: [Term]                 
    , documentLazyFields :: Map fields [ByteString]
    , documentLazyData  :: dat
    , documentDiffs :: Seq (DocumentDiff fields dat)
    } deriving (Show)

-- FIXME: unpack stuff to save space
data DocumentDiff fields dat
    = AddTerm ByteString {-# UNPACK #-} !Int
    | DelTerm ByteString {-# UNPACK #-} !Int
    | AddTerms [(ByteString, Int)]
    | DelTerms [(ByteString, Int)]
    | AddPosting ByteString {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    | DelPosting ByteString {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    | AddPostings [(ByteString, Int, Int)]
    | DelPostings [(ByteString, Int, Int)]
    | AddValue {-# UNPACK #-} !Int ByteString
    | DelValue {-# UNPACK #-} !Int
    | SetData dat
    | ClearTerms
    | ClearValues
    deriving (Show)

type Pos  = Word32


data Term = Term    ByteString [Pos] -- ^ a single term w/ or wo/
                                     --   positional information
          | RawText ByteString       -- ^ a text to be parsed as terms
  deriving (Eq, Show)

-- * Stemming related types
-- --------------------------------------------------------------------

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
