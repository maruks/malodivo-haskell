{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, RecordWildCards #-}

module Malodivo
  (splitAllBills,
   encodeResult,
   decodeInput,
   Bill (..),
   Category (..),
   District (..),
   Payment (..)) where

import Data.Maybe as Maybe
import Data.Map.Strict as Map
import Data.List as List
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Encode.Pretty (encodePretty)

data Category = Defense | Welfare | Science deriving (Show, Eq, Read, Ord, Generic, Enum)

data Bill = Bill { name :: String
                 , category :: Category
                 , amount :: Amount } deriving (Show, Eq, Generic)

data CategoryFunding = CategoryFunding { category :: Category
                                       , amount :: Amount } deriving (Show, Eq, Generic)

data BillFunding = BillFunding { bill :: String
                               , amount :: Amount } deriving (Show, Eq, Generic)

data CategoryCap = CategoryCap { category :: Category
                               , amount :: Amount } deriving (Show, Eq, Generic)

data DistrictJson = DistrictJson { dName :: String
                                 , availableFunds :: Amount
                                 , categoryDefaultFunding :: [CategoryFunding]
                                 , billSpecificFunding :: [BillFunding]
                                 , caps :: [CategoryCap] } deriving (Show, Eq, Generic)

data Input = Input { bills :: [Bill]
                   , districts :: [DistrictJson] } deriving (Show, Eq, Generic)

type Amount = Int

data District = District { distrName :: String
                         , funds :: Amount
                         , categoryFunding :: Map Category Amount
                         , billFunding :: Map String Amount
                         , categoryCaps :: Map Category Amount} deriving (Show, Eq, Ord)

data Payment = Payment { districtName :: String
                       , billName :: String
                       , paymentAmount :: Amount
                       , paymentCategory :: Category} deriving (Show, Eq, Generic)

data Output = Output {payments :: [Payment]} deriving (Show, Eq, Generic)

instance FromJSON Category
instance FromJSON Bill
instance FromJSON CategoryFunding
instance FromJSON BillFunding
instance FromJSON CategoryCap
instance FromJSON DistrictJson
instance FromJSON Input
instance ToJSON Payment
instance ToJSON Category
instance ToJSON Output

decodeInput :: B.ByteString -> Input
decodeInput = fromJust . decode

jsonToDistrict :: DistrictJson -> District
jsonToDistrict DistrictJson{..} =
  let categoryFunding = Map.fromList $ List.map (\CategoryFunding{..} -> (category, amount)) categoryDefaultFunding
      billFunding = Map.fromList $ List.map (\BillFunding{..} -> (bill, amount)) billSpecificFunding
      categoryCaps = Map.fromList $ List.map (\CategoryCap{..} -> (category, amount)) caps
  in District dName availableFunds categoryFunding billFunding categoryCaps

fundingAvailable :: District -> Bill -> Amount
fundingAvailable District{..} Bill{..} =
  let catFund = categoryFunding ! category
      billFund = fromMaybe catFund $ billFunding !? name
      cap = fromMaybe (maxBound :: Int) $ categoryCaps !? category
  in List.minimum [funds, billFund, cap]

subtractFunds :: District -> Amount -> Category -> District
subtractFunds d@District{..} amount category = d {funds = funds - amount,
                                                  categoryCaps = Map.adjust (subtract amount) category categoryCaps}

payment :: Double -> Bill -> District -> (District, Payment)
payment coeff bill@Bill{..} district@District{..} =
  let avail = fundingAvailable district bill
      amount = truncate (coeff * fromIntegral avail)
  in (subtractFunds district amount category, Payment distrName name amount category)

splitBill :: [District] -> Bill -> [(District, Payment)]
splitBill districts bill@Bill{..} =
  let payingDistricts = List.filter (\d -> fundingAvailable d bill > 0) districts
      notPaying = List.filter (\d -> fundingAvailable d bill == 0) districts
      totalAvailable = List.sum $ List.map (`fundingAvailable` bill) payingDistricts
      coeff = fromIntegral amount / fromIntegral (max amount totalAvailable)
      zeroPayments = (\d@District{..} -> (d, Payment distrName name 0 category)) <$> notPaying
      nonZeroPayments = payment coeff bill <$> payingDistricts
  in zeroPayments ++ nonZeroPayments

payAll :: ([District], [Payment]) -> Bill -> ([District], [Payment])
payAll (districts, payments) bill =
  let rs = splitBill districts bill
  in (List.map fst rs, payments ++ List.map snd rs)

balancePayments :: District -> [Bill] -> Category -> District
balancePayments district@District{..} bills cat =
  case categoryCaps !? cat of
    Nothing -> district
    Just amountAvailable ->
      let billsToPay = List.filter (\Bill{..} -> cat == category && (0 < Maybe.fromMaybe 1 (billFunding !? name))) bills
          categoryAmount = categoryFunding ! cat
          billAmounts = List.map (\Bill{..} -> Maybe.fromMaybe categoryAmount (billFunding !? name)) billsToPay
          totalToPay = List.sum billAmounts
          coeff = fromIntegral amountAvailable / fromIntegral totalToPay
          billNames = name <$> billsToPay
          updatedAmounts = List.map (\a -> truncate $ coeff * fromIntegral a) billAmounts
          newBillFunding = Map.union (Map.fromList $ zip billNames updatedAmounts) billFunding
      in if amountAvailable < totalToPay
         then district {billFunding =   newBillFunding}
         else district

balanceDistrictPayments :: [Bill] -> District -> District
balanceDistrictPayments bills district = List.foldl' (\d c -> balancePayments d bills c) district [Defense .. Science]

splitAllBills :: [District] -> [Bill] -> [Payment]
splitAllBills districts bills =
  let bills' = modifyBillAmounts districts bills
      balanced = List.map (balanceDistrictPayments bills') districts
  in snd $ List.foldl' payAll (balanced, []) bills'

modifyBillAmounts :: [District] -> [Bill] -> [Bill]
modifyBillAmounts districts bills =
  let totalBill = List.sum $ (amount:: (Bill->Amount)) <$> bills
      totalFunds = List.sum $ funds <$> districts
      coeff = fromIntegral totalFunds / fromIntegral totalBill
  in if totalFunds < totalBill
     then List.map (\b@Bill{..} -> (b {amount = truncate $ coeff * fromIntegral amount}) :: Bill ) bills
     else bills

sortPayments :: Payment -> Payment -> Ordering
sortPayments Payment{districtName = d1, paymentAmount = a1} Payment{districtName = d2, paymentAmount = a2} =
  let c1 = compare d1 d2
      c2 = compare a1 a2
  in c1 <> c2

encodeResult :: Input -> B.ByteString
encodeResult Input{..} =
  let ds = jsonToDistrict <$> districts
      ps = splitAllBills ds bills
  in encodePretty $ Output $ List.sortBy sortPayments ps
