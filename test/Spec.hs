module Main (main) where

import Test.Hspec
import Data.Time.Calendar (fromGregorian)

import Api.Reports (periodEndDates)

main :: IO ()
main = hspec $ do
  describe "periodEndDates" $ do
    it "generates monthly end dates" $
      periodEndDates (fromGregorian 2025 1 1) (fromGregorian 2025 3 31) "monthly"
        `shouldBe`
        [ fromGregorian 2025 1 31
        , fromGregorian 2025 2 28
        , fromGregorian 2025 3 31
        ]

    it "generates weekly end dates" $
      periodEndDates (fromGregorian 2025 1 1) (fromGregorian 2025 1 21) "weekly"
        `shouldBe`
        [ fromGregorian 2025 1 7
        , fromGregorian 2025 1 14
        , fromGregorian 2025 1 21
        ]

    it "generates daily end dates" $
      periodEndDates (fromGregorian 2025 1 1) (fromGregorian 2025 1 3) "daily"
        `shouldBe`
        [ fromGregorian 2025 1 1
        , fromGregorian 2025 1 2
        , fromGregorian 2025 1 3
        ]
