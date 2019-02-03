{-# LANGUAGE RecordWildCards #-}

import Test.Hspec
import Test.QuickCheck
import Malodivo

import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe as Maybe

genAmount :: Int -> Int -> Gen Int
genAmount min limit =
  choose (min, limit)

instance Arbitrary Category where
  arbitrary = do
    i <- arbitrary :: Gen Int
    return $ toEnum (i `mod` 3)

instance Arbitrary Bill where
  arbitrary = do
    name <- arbitrary
    category <- arbitrary
    amount <- genAmount 10 100000
    return $ Bill name category amount

instance Arbitrary District where
  arbitrary = do
    name <- arbitrary
    funds <- genAmount 100 500000
    bNames <- arbitrary :: Gen [Int]
    bAmounts <- listOf $ genAmount 0 100000
    catCaps <- vectorOf 3 $ genAmount 1000 500000
    cats <- arbitrary :: Gen [Category]
    cAmounts <- listOf $ genAmount 100 10000
    let categoryFunding = Map.fromList $ List.zip [Defense .. Science] catCaps
        categoryCaps = Map.fromList $ List.zip cats cAmounts
        billFunding =
          Map.fromList $
          List.zip (List.map (\b -> "bill-" ++ show (b `mod` 100)) bNames) bAmounts
    return $ District name funds categoryFunding billFunding categoryCaps

newtype TestInput =
  TestInput ([Bill], [District])
  deriving (Show)

instance Arbitrary TestInput where
  arbitrary = do
    bills <- listOf1 arbitrary :: Gen [Bill]
    districts <- listOf1 arbitrary :: Gen [District]
    let renamedBills =
          List.map
            (\(i, b) -> b { name = "bill-" ++ show i}) $
          List.zip [1 ..] bills
        billNames = Set.fromList $ List.map name renamedBills
    let renamedDistricts =
          List.map
            (\(i, d@District {..}) -> d { distrName = "district-" ++ show i , billFunding = Map.restrictKeys billFunding billNames}) $
          List.zip [1 ..] districts
    return $ TestInput (renamedBills, renamedDistricts)

billsMap:: [Bill] -> Map String Bill
billsMap bills = Map.fromList $ List.zip (name <$> bills) bills

districtsMap:: [District] -> Map String District
districtsMap districts =
  Map.fromList $ List.zip (distrName <$> districts) districts

main :: IO ()
main =
  hspec $
  describe "List of payments" $
  do it "Has correct size (number of bills multiplied by number of districts)" $
       property $
       \(TestInput (bills, districts)) ->
          length bills * length districts ==
          length (splitAllBills districts bills)
     it "All bills have unique names" $
       property $
         \(TestInput (bills, districts)) ->
           let names = name <$> bills
           in List.nub names == names
     it "All districts have unique names" $
       property $
         \(TestInput (bills, districts)) ->
           let names = distrName <$> districts
           in List.nub names == names
     it "All payments are non-negative" $
       property $
       \(TestInput (bills, districts)) ->
          List.all (>= 0) $ List.map paymentAmount $ splitAllBills districts bills
     it "Has payment for each bill and district" $
       property $
       \(TestInput (bills, districts)) ->
          let payments = splitAllBills districts bills
              billNames = List.map name bills
              districtNames = List.map distrName districts
              billsDistricts = (,) <$> billNames <*> districtNames
              paymentsMap = Map.fromList $ List.foldl' (\a Payment{..} -> ((billName,districtName), True) : a ) [] payments
          in List.all (`Map.member` paymentsMap) billsDistricts
     it "All payments have correct category" $
       property $
       \(TestInput (bills, districts)) ->
          let payments = splitAllBills districts bills
              bMap = billsMap bills
          in List.all
               (\Payment {..} -> paymentCategory == category (bMap ! billName))
               payments
     it "Total amount each district has paid doesn't exceed it's funds" $
       property $
       \(TestInput (bills, districts)) ->
          let payments = splitAllBills districts bills
              districtNames = List.map distrName districts
              dMap = districtsMap districts
              totalPaid d =
                List.sum $
                List.map paymentAmount $
                List.filter (\Payment {..} -> districtName == d) payments
          in List.all (\d -> totalPaid d <= funds (dMap ! d)) districtNames
     it "Total amount paid by each district doesn't exceed category limit" $
       property $
       \(TestInput (bills, districts)) ->
         let payments = splitAllBills districts bills
             dMap = districtsMap districts
             categoryCap districtName category = categoryCaps (dMap ! districtName) ! category
             categoryPayments distrName category = List.filter (\Payment{..} -> districtName == distrName && paymentCategory == category) payments
             totalPaid districtName category = List.sum $ List.map paymentAmount $ categoryPayments districtName category
             distrCategories = List.concatMap (\District{..} -> List.map (\category -> (distrName, category)) $ Map.keys categoryCaps) districts
         in List.all (\(districtName, category) -> totalPaid districtName category <= categoryCap districtName category) distrCategories
     it "Total amount paid for each bill doesn't exceed it's amount (Article XIV)" $
       property $
       \(TestInput (bills, districts)) ->
         let payments = splitAllBills districts bills
             billPayments billName = List.filter (\Payment{billName = name} -> name == billName) payments
             totalPaid billName = List.sum $ List.map paymentAmount $ billPayments billName
         in List.all (\Bill{..} -> totalPaid name <= amount) bills
     it "Each payment doesn't exceed district's category or bill funding amount" $
       property $
       \(TestInput (bills, districts)) ->
         let payments = splitAllBills districts bills
             dMap = districtsMap districts
             bMap = billsMap bills
             maxPayment District {..} Bill{..} = Maybe.fromMaybe (categoryFunding ! category) $ billFunding !? name
         in List.all (\Payment{..} -> paymentAmount <= maxPayment (dMap ! districtName) (bMap ! billName)) payments
     it "Total amount of funds collected doesn't exceed total amount of available funds" $
       property $
       \(TestInput (bills, districts)) ->
         let payments = splitAllBills districts bills
             totalPaid = List.sum $ List.map paymentAmount payments
             totalAvailable = List.sum $ List.map funds districts
         in totalPaid <= totalAvailable
