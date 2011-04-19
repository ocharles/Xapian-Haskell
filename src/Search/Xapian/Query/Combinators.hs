module Search.Xapian.Query.Combinators
      (
      -- * Combinators
        scale, eliteSet, or, and, xor
      , andMaybe, andNot, filter, near
      , synonyms, phrase, greaterEqual
      , lowerEqual, inRange
      ) where
       
import Prelude hiding (or, and, filter)
import Search.Xapian.Internal.Types

-- | Most likely you want to import this module as a qualified module, i.e.
-- 
-- > import qualified Search.Xapian.Query.Combinators as Q

greaterEqual :: ValueNumber -> Value -> Query
greaterEqual valno val = Nullary $ OpValueGE valno val

lowerEqual :: ValueNumber -> Value -> Query
lowerEqual   valno val = Nullary $ OpValueLE valno val

inRange :: ValueNumber -> [Value] -> Query
inRange valno vals = Nullary $ OpValueRange valno vals

--
scale :: Double -> Query -> Query
scale s = Unary (OpScaleWeight s)

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

eliteSet :: Query -> Query -> Query
eliteSet = Binary OpEliteSet

--
synonyms :: [Query] -> Query
synonyms = Multi OpSynonym

phrase :: Int -> [Query] -> Query
phrase windowSize = Multi (OpPhrase windowSize)
