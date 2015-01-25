{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Hierarchy where

import           Control.Applicative
import           Control.Foldl          (Fold (..))
import qualified Control.Foldl          as CF
import           Control.Lens           (Rewrapped, Traversable, Unwrapped, At, FoldableWithIndex,
                                         makeLenses,
                                         traversed, view, _Unwrapping, _Wrapped)
import qualified Control.Lens           as Lens
import           Control.Lens.Operators
import qualified Data.Foldable          as F
import qualified Data.Map.Lazy          as M
import           Data.Maybe
import           Data.Monoid            (Monoid (..), Sum (Sum), mempty)
import           Data.Profunctor
import           Data.Set               (Set)
import           Data.Text              (Text)
import qualified Text.Show.Pretty as Pr


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


trades :: [Trade]
trades = [ Trade "1CAD" (M.fromList [("BOOK", "A")]) $ M.fromList [("ONE", 1), ("TWO", 107), ("THREE", 1)]
         , Trade "1CAD" (M.fromList [("BOOK", "A")]) $ M.fromList [("ONE", 2), ("TWO", 118), ("FOUR", 40)]
         , Trade "1CAD" (M.fromList [("BOOK", "B")]) $ M.fromList [("ONE", 3), ("TWO", 109), ("THREE", 70)]
         , Trade "1CAD" (M.fromList [("BOOK", "A")]) $ M.fromList [("ONE", 4), ("TWO", 114), ("FOUR", 20)]
         , Trade "1CAD" (M.fromList [("BOOK", "D")]) $ M.fromList [("ONE", 5), ("TWO", 106), ("THREE", 90)]
         , Trade "1CAD" (M.fromList [("BOOK", "A")]) $ M.fromList [("ONE", 6), ("TWO", 112), ("FOUR", 10)]
         , Trade "1CAD" (M.fromList [("BOOK", "C")]) $ M.fromList [("ONE", 1), ("TWO", 102), ("THREE", 40)]
         , Trade "1CAD" (M.fromList [("BOOK", "A")]) $ M.fromList [("ONE", 7), ("TWO", 115), ("FOUR", 30)]
         , Trade "1CAD" (M.fromList [("BOOK", "B")]) $ M.fromList [("ONE", 1), ("TWO", 102), ("THREE", 50)]
         ]

--trades ^.. traversed.filtered (has $ measures.ix "ONE")

foldMon
  :: (Monoid s, Rewrapped s s, Traversable f,
      Unwrapped s ~ Double) =>
     (Double -> s) -> f Trade -> M.Map Measure Double
foldMon mon t =
  view _Wrapped <$> F.foldr (M.unionWith mappend) M.empty (fmap (view (_Unwrapping mon)) <$> t ^.. Lens.traversed.measures)

range :: (Ord a) => CF.Fold a (Maybe (a, a))
range = fmap (Lens.sequenceOf Lens.both) $ (,) <$> CF.minimum <*> CF.maximum

-- let one = CF.pretraverse (measures.ix "ONE") CF.sum
-- let two = CF.pretraverse (measures.ix "TWO") range
-- CF.fold ((,) <$> one <*> two) trades
-- (3.0,Just (10.0,11.0))

-- CF.fold (CF.pretraverse (measures) keys) trades
keys
  :: Ord k =>
     Fold (M.Map k a) (Set k)
keys = Fold M.union M.empty M.keysSet

counts :: Ord k => Fold (M.Map k m) (M.Map k Integer)
counts = Fold step M.empty id
  where
  step acc m = M.foldrWithKey (M.insertWith (+)) (fmap (const 1) m) acc

overMaps :: (Ord k) => Fold a b -> Fold (M.Map k a) (M.Map k b)
overMaps (Fold step begin done) = Fold step' M.empty (fmap done)
  where
  step' acc m = M.foldrWithKey insert acc m
  insert k el acc = M.insert k (step (fromMaybe begin $ M.lookup k acc) el) acc


overFoldable :: (Ord k) => Fold a b -> Fold (M.Map k a) (M.Map k b)
-- overFoldable :: (Ord i, At (f i a), FoldableWithIndex i (f i))
--              => Fold a b -> Fold (f i a) (f i b)
overFoldable (Fold step begin done) = Fold step' M.empty (fmap done)
  where
  step' acc m = Lens.ifoldr insert acc m
  insert k el acc = Lens.at k %~ return . flip step el . fromMaybe begin $ acc
  
data Stats a = Stats {
    _min :: Maybe a
  , _max :: Maybe a
  , _avg :: Double
  } deriving (Show, Eq, Ord)

grouped :: (Ord a) => Fold a b -> Fold a (M.Map a b)
grouped (Fold step begin done) = Fold step' M.empty (fmap done)
  where
  step' acc k = Lens.at k %~ return . flip step k . fromMaybe begin $ acc

stats :: Fold Double (Stats Double)
stats = Stats <$> CF.minimum
              <*> CF.maximum
              <*> ((/) <$> CF.sum <*> CF.genericLength)
            


test = CF.fold fld trades
  where
  fld = (,) <$> fmap M.toList (CF.pretraverse dimensions (overFoldable CF.set))
            <*> fmap M.toList (CF.pretraverse (measures) (overFoldable ((,) <$> CF.sum <*> stats)))
