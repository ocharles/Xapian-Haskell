module Search.Xapian.Query.Combinators
      (
      -- * Combinators
        queryAll, queryAny
      , scale, eliteSet, or, and, xor
      , andMaybe, andNot, filter, near
      , synonyms, phrase, greaterEqual
      , lowerEqual, inRange
      ) where
       
import Prelude hiding (or, and, filter)
import Data.ByteString (ByteString)
import Search.Xapian.Internal.Types
import Search.Xapian.Query

-- | Most likely you want to import this module as a qualified module, i.e.
-- 
-- > import qualified Search.Xapian.Query.Combinators as Q

queryAll :: Queryable s =>  [s] -> Query
queryAll [] = MatchAll
queryAll xs = foldr1 and $ map query xs

queryAny :: Queryable s => [s] -> Query
queryAny [] = MatchNothing
queryAny xs = foldr1 eliteSet $ map query xs

greaterEqual :: ValueNumber -> Value -> Query
greaterEqual valno val = Nullary $ OpValueGE valno val

lowerEqual :: ValueNumber -> Value -> Query
lowerEqual   valno val = Nullary $ OpValueLE valno val

inRange :: ValueNumber -> Value -> Value -> Query
inRange valno lower upper = Nullary $ OpValueRange valno lower upper

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

eliteSet :: Query -> Query -> Query
eliteSet = Binary OpEliteSet

--
synonyms :: [Query] -> Query
synonyms = Multi OpSynonym


near :: Queryable term => Int -> [term] -> Query
near distance = MultiFlat (OpNear distance) . map (unatom . query)

phrase :: Queryable term => Int -> [term] -> Query
phrase windowSize = MultiFlat (OpPhrase windowSize) . map (unatom . query)

unatom (Atom bs) = bs
unatom _         = error "erroneous call of unatom"
