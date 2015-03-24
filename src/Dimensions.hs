{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dimensions where

import           Control.Applicative
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Pipes
import           Pipes.Csv           (HasHeader (..))
import qualified Pipes.Csv           as Csv

import qualified Pipes.ByteString    as PBS
import           System.IO

main :: IO ()
main =
  withFile "data/dimensions.csv"  ReadMode  $ \hIn  ->
     runEffect $ for (Csv.decode NoHeader (PBS.fromHandle hIn))
                 $ \row -> case row of
                       Right (r :: (MeasureName, MeasureName)) -> liftIO $ print r
                       Left e -> liftIO $ print e

type MeasureName = Text

--------------------------------------------------------------------------------
measureMapping :: [(MeasureName, MeasureName)]
measureMapping = [
    ("PL Theta", "TimeDecay")
  , ("PL FX Cross", "FinalResidual")
  , ("PL FX/IR Cross", "FXIRResidual")
  , ("PL FX Delta", "TotalFXEstimatedPnL")
  , ("PL FX Higher", "FXResidual")
  , ("PL Rates Higher", "IRResidual")
  , ("PL Rates Higher", "IRResidual")
  , ("PL New Trd", "NewDealsAdjustment")
  , ("PL New Trd", "NewDealsAdjustment")
  , ("PL New Trd", "NewDealsAdjustment")
  , ("PVOpen", "MTM-Cut PV")
  , ("('PVOpen')BaseCCY", "")
  , ("PV", "ProcessedPVClose")
  , ("('PV')BaseCCY", "")
  , ("CashReceived", "")
  , ("('CashReceived')BaseCCY", "Cash Base Amount")
  ]

legacyToTargetMeasures, targetToLegacyMeasures :: Map MeasureName MeasureName
legacyToTargetMeasures = Map.fromList measureMapping
targetToLegacyMeasures = Map.fromList [(v, k) | (k, v) <- measureMapping]

--------------------------------------------------------------------------------
dimensionMapping :: [(MeasureName, MeasureName)]
dimensionMapping = [
    ("Book.BookName","BackOfficeBook")
  , ("Book.BusinessUnitName","BusinessUnitName")
  , ("Instruments.MaturityDate","Maturity Date")
  , ("Positions.AdminID","AdminNumber")
  , ("Positions.DealID","DealId")
  , ("Positions.TSecID","n/a")
  , ("Instruments.Product","TradeType")
  , ("Instruments.SettlementCurrency","Base Currency")
  , ("Deal.TradeDate","TradeDate")
  ]

legacyToTargetDimensions, targetToLegacyDimensions :: Map MeasureName MeasureName
legacyToTargetDimensions = Map.fromList dimensionMapping
targetToLegacyDimensions = Map.fromList [(v, k) | (k, v) <- dimensionMapping]

books, bus, mdates, tdates, tradeTypes :: [Text]
books = ["JPYD", "1EMD", "ABCD", "XYZZ"]
bus = ["BU1", "BU2", "BU3", "BU4"]
mdates = Text.intercalate "/" <$> [Text.pack . show <$> [y, m, d] | y <- [(15::Int)..16]
                                                                  , m <- [3..8]
                                                                  , d <- [1..28]]
tdates = Text.intercalate "/" <$> [Text.pack . show <$> [y, m, d] | y <- [(15::Int)..16]
                                                                  , m <- [3..8]
                                                                  , d <- [1..28]]
tradeTypes = ["FX Swap", "FX Spot", "FX Option"]
--tdates = Text.intersperse '/' <$> [(Text.pack . show) <$> [y, m, d] | y <- [15..16], m <- [3..8], d <- [1..28]]

dimChoices :: Map MeasureName [Text]
dimChoices = Map.fromList [
    ("Book.BookName", books)
  , ("Book.BusinessUnitName",bus)
  , ("Instruments.Product", tradeTypes)
  , ("Instruments.MaturityDate", mdates)
  , ("Deal.TradeDate", tdates)
  ]
