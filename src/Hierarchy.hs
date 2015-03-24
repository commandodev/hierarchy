{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module Hierarchy where

import           Control.Applicative
import           Control.Foldl          (Fold (..))
import qualified Control.Foldl          as CF
import           Control.Lens           (At, FoldableWithIndex, Rewrapped,
                                         Traversable, Unwrapped, Wrapped,
                                         makeLenses, traversed, view,
                                         _Unwrapping, _Wrapped)
import qualified Control.Lens           as Lens
import           Control.Lens.Operators
import           Control.Monad          (forM)
import           Data.ByteString        (ByteString)
import qualified Data.Csv               as Csv
import qualified Data.Foldable          as F
import qualified Data.HashMap.Strict    as HM
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Data.Monoid            (Monoid (..), Sum (Sum), mempty, (<>))

import           Data.Set               (Set)
import           Data.String            (fromString)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable
import qualified Data.Vector            as V
import           Dimensions
import           Pipes
import           Pipes.Csv              as PCsv
import           Pipes.Csv.Encoding     as PCsv
import qualified Pipes.Extras           as P
import           Test.QuickCheck        (Arbitrary)
import qualified Test.QuickCheck        as QC
-- import qualified Test.QuickCheck.Monadic as QC
import qualified Text.Show.Pretty       as Pr


type Dim = Text
type Measure = Text
type TradeId = Int

newtype Dimensions = Dimensions { _unDims :: Map Dim Text } deriving (Show, Typeable)

mkDims :: [(Dim, Text)] -> Dimensions
mkDims = Dimensions . Map.fromList

type Measures   = Map Measure Double

data Hierarchy a =
    Node Dim [Hierarchy a]
  | Leaf Measure a


data Trade = Trade {
    _tId        :: TradeId
  , _dimensions :: Dimensions
  , _measures   :: Measures
  } deriving Show


instance Arbitrary Trade where
  arbitrary = do
    tid <- QC.arbitrary
    dims <- forM dimensionMapping $ \(fst -> dim) ->
      (dim,) <$> maybe (fromString <$> QC.arbitrary) QC.elements (Map.lookup dim dimChoices)
    ms <- forM measureMapping $ \(fst -> m) -> (m,) <$> QC.arbitrary
    return $ Trade tid (mkDims dims)  (Map.fromList ms)

makeLenses ''Trade


trades :: [Trade]
trades = [ Trade 1 (mkDims [("BOOK", "A")]) $ Map.fromList [("ONE", 1), ("TWO", 107), ("THREE", 1)]
         , Trade 2 (mkDims [("BOOK", "A")]) $ Map.fromList [("ONE", 2), ("TWO", 118), ("FOUR", 40)]
         , Trade 3 (mkDims [("BOOK", "B")]) $ Map.fromList [("ONE", 3), ("TWO", 109), ("THREE", 70)]
         , Trade 4 (mkDims [("BOOK", "A")]) $ Map.fromList [("ONE", 4), ("TWO", 114), ("FOUR", 20)]
         , Trade 5 (mkDims [("BOOK", "D")]) $ Map.fromList [("ONE", 5), ("TWO", 106), ("THREE", 90)]
         , Trade 6 (mkDims [("BOOK", "A")]) $ Map.fromList [("ONE", 6), ("TWO", 112), ("FOUR", 10)]
         , Trade 7 (mkDims [("BOOK", "C")]) $ Map.fromList [("ONE", 1), ("TWO", 102), ("THREE", 40)]
         , Trade 8 (mkDims [("BOOK", "A")]) $ Map.fromList [("ONE", 7), ("TWO", 115), ("FOUR", 30)]
         , Trade 9 (mkDims [("BOOK", "B")]) $ Map.fromList [("ONE", 1), ("TWO", 102), ("THREE", 50)]
         ]

--trades ^.. traversed.filtered (has $ measures.ix "ONE")

foldMon
  :: (Monoid s, Rewrapped s s, Traversable f,
      Unwrapped s ~ Double) =>
     (Double -> s) -> f Trade -> Map Measure Double
foldMon mon t =
  view _Wrapped <$> F.foldr (Map.unionWith mappend) Map.empty (fmap (view (_Unwrapping mon)) <$> t ^.. Lens.traversed.measures)

range :: (Ord a) => CF.Fold a (Maybe (a, a))
range = fmap (Lens.sequenceOf Lens.both) $ (,) <$> CF.minimum <*> CF.maximum

-- let one = CF.pretraverse (measures.ix "ONE") CF.sum
-- let two = CF.pretraverse (measures.ix "TWO") range
-- CF.fold ((,) <$> one <*> two) trades
-- (3.0,Just (10.0,11.0))

-- CF.fold (CF.pretraverse (measures) keys) trades
keys
  :: Ord k =>
     Fold (Map k a) (Set k)
keys = Fold Map.union Map.empty Map.keysSet

counts :: Ord k => Fold (Map k m) (Map k Integer)
counts = Fold step Map.empty id
  where
  step acc m = Map.foldrWithKey (Map.insertWith (+)) (fmap (const 1) m) acc

overMaps :: (Ord k) => Fold a b -> Fold (Map k a) (Map k b)
overMaps (Fold step begin done) = Fold step' Map.empty (fmap done)
  where
  step' acc m = Map.foldrWithKey insert acc m
  insert k el acc = Map.insert k (step (fromMaybe begin $ Map.lookup k acc) el) acc


overFoldable :: (Ord k) => Fold a b -> Fold (Map k a) (Map k b)
-- overFoldable :: (Ord i, At (f i a), FoldableWithIndex i (f i))
--              => Fold a b -> Fold (f i a) (f i b)
overFoldable (Fold step begin done) = Fold step' Map.empty (fmap done)
  where
  step' acc m = Lens.ifoldr insert acc m
  insert k el acc = Lens.at k %~ return . flip step el . fromMaybe begin $ acc

data Stats a = Stats {
    _min :: Maybe a
  , _max :: Maybe a
  , _avg :: Double
  } deriving (Show, Eq, Ord)

-- instance ToNamedRecord Stats where
--     toNamedRecord (Stats name age) = namedRecord [
--         "name" .= name, "age" .= age]

groupedBy :: (Ord k) => (a -> k) -> Fold a b -> Fold a (Map k b)
groupedBy f (Fold step begin done) = Fold step' Map.empty (fmap done)
  where
  step' acc k = Lens.at (f k) %~ return . flip step k . fromMaybe begin $ acc


grouped :: (Ord a) => Fold a b -> Fold a (Map a b)
grouped = groupedBy id

stats :: Fold Double (Stats Double)
stats = Stats <$> CF.minimum
              <*> CF.maximum
              <*> ((/) <$> CF.sum <*> CF.genericLength)

produceNTrades :: Int -> Producer Trade IO ()
produceNTrades 0 = return ()
produceNTrades n = do
  t <- liftIO $ QC.generate QC.arbitrary
  yield t
  produceNTrades (n - 1)

test :: IO ()
test = putStrLn =<< Pr.ppShow <$> P.fold fld (produceNTrades 10000)
  where
  fld = groupedBy (Lens.firstOf (dimensions . Lens.to _unDims . Lens.ix "Book.BookName"))
                  (CF.pretraverse (measures) (overFoldable ((,) <$> CF.sum <*> stats)))
-- foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
formatRecord :: Map k a
             -> (k -> [(ByteString, ByteString)])
             -> (a -> NamedRecord)
             -> Producer NamedRecord IO (Map k ())
formatRecord m kfn vfn = Map.traverseWithKey f m
  where
    f k a = yield $ Csv.namedRecord (kfn k) <> vfn a


formatFolded :: Map Text (Double, Stats Double) -> Csv.NamedRecord
formatFolded m = Map.foldlWithKey' f HM.empty m
  where
    f acc k (sm, (Stats mn mx av)) = acc <> Csv.namedRecord [
        toKV k "sum" sm
      --, toKV k "max" mx
      --, toKV k "min" mn
      , toKV k "avg" av
      ]
    toKV :: Csv.ToField v => Text -> Text -> v -> (ByteString, ByteString)
    toKV k sub v = (Csv.toField $ Text.concat [k, "_", sub]) ..= Csv.toField v
    (..=) = (Csv..=)

namedRecordToHeader :: HM.HashMap k v -> V.Vector (k, v)
namedRecordToHeader = V.fromList . HM.toList

resultsToCsv m = PCsv.encodeByName undefined
