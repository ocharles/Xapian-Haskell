-- | this module is not intended to be made visible
module Search.Xapian.Internal.Types
       ( XapianM (runXapian)

       , Error (..)
       , NativeError (..)

       , ReadWriteDB (..)
       , ReadOnlyDB (..)
       , castPtr
       , castForeignPtr

       , Query (..)
       , OpNullary (..)
       , OpUnary (..)
       , OpBinary (..)
       , OpMulti (..)
       , QueryPtr

       , DocumentPtr
       , DocumentId (..)
       , Wdf
       , ValueNumber
       , Value
       , Document (..)
       , Term (..)
       , Pos

       , StemPtr
       , Stemmer (..)
       ) where

import Foreign
import Data.ByteString (ByteString)
import Control.Monad
import Control.Monad.Trans (liftIO, MonadIO)

import Search.Xapian.Internal.FFI


-- * Xapian's own monad :)
-- --------------------------------------------------------------------

newtype XapianM a = XapianM {runXapian :: IO a}

instance Monad XapianM where
    return  = XapianM . return
    x >>= f = XapianM $
               do x' <- runXapian x
                  runXapian $ f x'

instance MonadIO XapianM where
    liftIO = XapianM

instance Functor XapianM where
    fmap f = XapianM . fmap f . runXapian


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

newtype ReadWriteDB = ReadWriteDB { rwDbPtr :: WritableDatabasePtr }
newtype ReadOnlyDB = ReadOnlyDB { roDbPtr :: DatabasePtr }

instance Show ReadWriteDB where
    show = ("ReadWriteDB " ++) . show . rwDbPtr

instance Show ReadOnlyDB where
    show = ("ReadOnlyDB " ++) . show . roDbPtr

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

-- * within document frequency
type Wdf         = Int

-- * document fields
type ValueNumber = Word32
type Value       = ByteString

-- | doc_id == 0 is invalid; what is the range of
newtype DocumentId = DocId { getDocId :: Word32 }
    deriving (Show, Eq)

-- | A @Document@
data Document
    = Document 
      { docPtr :: DocumentPtr
      , docId  :: DocumentId
      } deriving (Show)

data Term = Term    ByteString [Pos] -- ^ a single term w/ or wo/
                                     --   positional information
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
