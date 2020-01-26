module Lib
    ( Account (..)
    , traditionalIraYear
    , rothIraYear
    , compoundYearly
    ) where

stockGrowth = 0.07
effectiveIncomeTax = 0.32
longTermCapitalGainsTax = 0.15

data Account = Account { nonRetirement :: Double
                       , preTax :: Double
                       , postTax :: Double } deriving (Show)

applyNonRetirement :: (Double -> Double) -> Account -> Account
applyNonRetirement fn account =
  account { nonRetirement = fn (nonRetirement account) }

applyPreTax :: (Double -> Double) -> Account -> Account
applyPreTax fn account =
  account { preTax = fn (preTax account) }

applyPostTax :: (Double -> Double) -> Account -> Account
applyPostTax fn account =
  account { postTax = fn (postTax account) }

stockGrowthYear :: Account -> Account
stockGrowthYear account =
  applyNonRetirement (nonRetirementIncrease +) . applyPreTax (stockMultiplier *) . applyPostTax (stockMultiplier *) $ account
  where
    stockMultiplier = 1 + stockGrowth
    -- Capital gains tax is applied only to money outside the IRA
    nonRetirementIncrease = (nonRetirement account) * stockGrowth * (1 - longTermCapitalGainsTax)

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
