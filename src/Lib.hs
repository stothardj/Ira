{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Account (..)
    , traditionalIraYear
    , rothIraYear
    , compoundYearly
    , formatAccountRow
    , printAccountTable
    ) where

import Formatting (sformat, (%), (%.))
import Formatting.Formatters (left, fixed)
import qualified Data.Text.IO as TextIO

-- ASSUMPTIONS
-- How much stocks will grow each year. Stocks can only go up.
stockGrowth = 0.07
-- Effective income tax while working. Should be derived by looking at tax brackets. Not coding that here.
effectiveIncomeTax = 0.32
-- Capital gains for holding stocks/investments for atleast a year.
longTermCapitalGainsTax = 0.15
-- Effective income bracket while withdrawing from pre-tax savings.
preTaxWithdrawalIncomeTax = 0.28

data Account = Account { nonRetirement :: Double
                       , preTax :: Double
                       , postTax :: Double
                       , capitalGained :: Double } deriving (Show)

-- Simple update functions.
-- I'm sure there's a way to do this with lenses. Should learn them at some point.
applyNonRetirement :: (Double -> Double) -> Account -> Account
applyNonRetirement fn account =
  account { nonRetirement = fn (nonRetirement account) }

applyPreTax :: (Double -> Double) -> Account -> Account
applyPreTax fn account =
  account { preTax = fn (preTax account) }

applyPostTax :: (Double -> Double) -> Account -> Account
applyPostTax fn account =
  account { postTax = fn (postTax account) }

applyCapitalGained ::(Double -> Double) -> Account -> Account
applyCapitalGained fn account =
  account { capitalGained  = fn (capitalGained account) }
-- end update functions.

stockGrowthYear :: Account -> Account
stockGrowthYear account =
  applyNonRetirement sfn . applyPreTax sfn . applyPostTax sfn . applyCapitalGained cfn $ account
  where
    stockMultiplier = 1 + stockGrowth
    sfn = (stockMultiplier *)
    capitalGained = stockGrowth * (nonRetirement account)
    cfn = (capitalGained +)

traditionalIraYear :: Double -> Double -> Account -> Account
traditionalIraYear investibleIncome iraContribution =
  applyNonRetirement (nonRetirementContribution +) . applyPreTax (iraContribution +) . stockGrowthYear
  where
    -- Tax is applied to only the money going into the non-retirement account
    nonRetirementContribution = (investibleIncome - iraContribution) * (1 - effectiveIncomeTax)

rothIraYear :: Double -> Double -> Account -> Account
rothIraYear investibleIncome iraContribution =
  applyNonRetirement (nonRetirementContribution +) . applyPostTax (iraContribution +) . stockGrowthYear
  where
    -- Tax is applied to all the money
    nonRetirementContribution = investibleIncome * (1 - effectiveIncomeTax) - iraContribution

compoundYearly :: Int -> (Account -> Account) -> Account -> [Account]
compoundYearly years fn = take years . iterate fn

withdrawableNonRetirement :: Account -> Double
withdrawableNonRetirement account =
  (nonRetirement account) - longTermCapitalGainsTax * (capitalGained account)
withdrawablePreTax :: Account -> Double
withdrawablePreTax account =
  (preTax account) * (1 - preTaxWithdrawalIncomeTax)
withdrawablePostTax :: Account -> Double
withdrawablePostTax account =
  (postTax account)

withdrawableNetAmount :: Account -> Double
withdrawableNetAmount account = wnr + wpret + wpostt
  where
    wnr = withdrawableNonRetirement account
    wpret = withdrawablePreTax account
    wpostt = withdrawablePostTax account

--                     0123456789012345|0123456789012345|0123456789012345|0123456789012345|
accountHeader =       " Non-retirement |       Post-tax |        Pre-Tax |  Capital gained|"
accountTableDivider = "--------------------------------------------------------------------"
accountColFormat = (left 15 ' ' %. fixed 2) % " |"
accountRowFormat = accountColFormat % accountColFormat % accountColFormat % accountColFormat
formatAccountRow account = sformat accountRowFormat (nonRetirement account) (postTax account) (preTax account) (capitalGained account)
printAccountTable accounts = do
  putStrLn accountHeader
  putStrLn accountTableDivider
  mapM_ (TextIO.putStrLn . formatAccountRow) accounts
  putStrLn accountTableDivider
  printFooterAmount "Net withdrawable amount: " (withdrawableNetAmount finalAccount)
  printFooterAmount "Withdrawable from non-retirement: " (withdrawableNonRetirement finalAccount)
  printFooterAmount "Withdrawable from post-tax: " (withdrawablePostTax finalAccount)
  printFooterAmount "Withdrawable from pre-tax: " (withdrawablePreTax finalAccount)
  where
    finalAccount = last accounts
    printFooterAmount label amount =
      TextIO.putStrLn $ sformat (label % (fixed 2)) amount

