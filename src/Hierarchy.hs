{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Hierarchy where

import           Control.Applicative
import Control.Foldl (Fold(..))
import qualified Control.Foldl       as CF
import           Control.Lens hiding (Fold)
import qualified Data.Foldable       as F
import qualified Data.Map.Lazy       as M
import           Data.Monoid         (Monoid (..), Sum (Sum))
import           Data.Profunctor
import Data.Set (Set)
import           Data.Text           (Text)


type Dim = String
type Measure = String
type Book = String

type Dimensions = M.Map Dim Text
type Measures   = M.Map Measure Double

data Hierarchy a = Node Dim [Hierarchy a]
                 | Leaf Measure a


data Trade = Trade {
    _book       :: Book
  , _dimensions :: Dimensions
  , _measures   :: Measures
  } deriving Show

makeLenses ''Trade


foldTrades :: [Trade] -> M.Map Measure Double -> M.Map Measure Double
foldTrades trades m = foldr fldMap m trades
  where
  fldMap t m = M.foldrWithKey f m (t ^. measures)
  f m n totals = M.insertWith (+) m n totals


test :: M.Map Measure Double
test = foldTrades trades M.empty


trades :: [Trade]
trades = [ Trade "1CAD" M.empty $ M.fromList [("ONE", 1), ("TWO", 10), ("THREE", 1)]
         , Trade "1CAD" M.empty $ M.fromList [("ONE", 1), ("TWO", 11), ("FOUR", 20)]
         , Trade "1CAD" M.empty $ M.fromList [("ONE", 1), ("TWO", 10), ("THREE", 0)]
         ]

--trades ^.. traversed.filtered (has $ measures.ix "ONE")

foldMon
  :: (Monoid s, Rewrapped s s, Traversable f,
      Unwrapped s ~ Double) =>
     (Double -> s) -> f Trade -> M.Map Measure Double
foldMon mon trades =
  view _Wrapped <$> F.foldr (M.unionWith mappend) M.empty (fmap (view (_Unwrapping mon)) <$> trades ^.. traversed.measures)

range :: (Ord a) => CF.Fold a (Maybe (a, a))
range = fmap (sequenceOf both) $ (,) <$> CF.minimum <*> CF.maximum

-- let one = CF.pretraverse (measures.ix "ONE") CF.sum
-- let two = CF.pretraverse (measures.ix "TWO") range
-- CF.fold ((,) <$> one <*> two) trades
-- (3.0,Just (10.0,11.0))

-- CF.fold (CF.pretraverse (measures) keys) trades
keys
  :: Ord k =>
     Fold (M.Map k a) (Set k)
keys = Fold M.union M.empty M.keysSet
