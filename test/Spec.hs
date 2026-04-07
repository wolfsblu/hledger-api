module Main (main) where

import Test.Hspec
import Data.Time.Calendar (fromGregorian)

import qualified Hledger as H

import Api.Reports (periodEndDates)
import Api.Transactions (applyFilters, applySorting)
import Api.Types.Sort

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

  describe "parseSortParam" $ do
    it "parses single ascending field" $
      parseSortParam "date"
        `shouldBe` Right [SortSpec SortDate Asc]

    it "parses single descending field" $
      parseSortParam "-description"
        `shouldBe` Right [SortSpec SortDescription Desc]

    it "parses multi-field sort" $
      parseSortParam "date,-description,status"
        `shouldBe` Right
          [ SortSpec SortDate Asc
          , SortSpec SortDescription Desc
          , SortSpec SortStatus Asc
          ]

    it "rejects unknown field" $
      parseSortParam "date,-invalid"
        `shouldBe` Left "Unknown sort field: invalid"

    it "parses amount field" $
      parseSortParam "-amount"
        `shouldBe` Right [SortSpec SortAmount Desc]

  describe "applyFilters" $ do
    let mkTxn date desc status tags postings = H.nulltransaction
          { H.tdate = date
          , H.tdescription = desc
          , H.tstatus = status
          , H.ttags = tags
          , H.tpostings = postings
          }
        mkPosting acct qty commodity = H.nullposting
          { H.paccount = acct
          , H.pamount = H.mixed [H.nullamt { H.aquantity = qty, H.acommodity = commodity }]
          }

        txn1 = mkTxn (fromGregorian 2025 1 15) "Grocery store" H.Cleared
                  [("food", ""), ("shop", "walmart")]
                  [mkPosting "expenses:food" 50 "USD", mkPosting "assets:bank" (-50) "USD"]
        txn2 = mkTxn (fromGregorian 2025 2 1) "Salary" H.Unmarked
                  [("income", "")]
                  [mkPosting "assets:bank" 3000 "USD", mkPosting "income:salary" (-3000) "USD"]
        txn3 = mkTxn (fromGregorian 2025 3 10) "Rent payment" H.Pending
                  [("housing", "")]
                  [mkPosting "expenses:rent" 1200 "EUR", mkPosting "assets:bank" (-1200) "EUR"]
        allTxns = [txn1, txn2, txn3]

        noFilter = applyFilters Nothing Nothing [] Nothing [] [] Nothing Nothing

    describe "status filter" $ do
      it "filters cleared transactions" $
        applyFilters Nothing Nothing [] Nothing [H.Cleared] [] Nothing Nothing allTxns
          `shouldBe` [txn1]

      it "filters unmarked transactions" $
        applyFilters Nothing Nothing [] Nothing [H.Unmarked] [] Nothing Nothing allTxns
          `shouldBe` [txn2]

      it "filters pending transactions" $
        applyFilters Nothing Nothing [] Nothing [H.Pending] [] Nothing Nothing allTxns
          `shouldBe` [txn3]

      it "filters multiple statuses (OR)" $
        applyFilters Nothing Nothing [] Nothing [H.Cleared, H.Pending] [] Nothing Nothing allTxns
          `shouldBe` [txn1, txn3]

    describe "account filter" $ do
      it "filters by single account" $
        applyFilters Nothing Nothing ["expenses:food"] Nothing [] [] Nothing Nothing allTxns
          `shouldBe` [txn1]

      it "filters by multiple accounts (OR)" $
        applyFilters Nothing Nothing ["expenses:food", "income:salary"] Nothing [] [] Nothing Nothing allTxns
          `shouldBe` [txn1, txn2]

      it "empty list returns all" $
        applyFilters Nothing Nothing [] Nothing [] [] Nothing Nothing allTxns
          `shouldBe` allTxns

    describe "tag filter" $ do
      it "matches tag by name only (case-insensitive)" $
        applyFilters Nothing Nothing [] Nothing [] ["Food"] Nothing Nothing allTxns
          `shouldBe` [txn1]

      it "matches tag by name:value pair" $
        applyFilters Nothing Nothing [] Nothing [] ["shop:walmart"] Nothing Nothing allTxns
          `shouldBe` [txn1]

      it "rejects non-matching tag value" $
        applyFilters Nothing Nothing [] Nothing [] ["shop:target"] Nothing Nothing allTxns
          `shouldBe` []

      it "filters by multiple tags (OR)" $
        applyFilters Nothing Nothing [] Nothing [] ["food", "housing"] Nothing Nothing allTxns
          `shouldBe` [txn1, txn3]

    describe "amount filter" $ do
      it "minAmount includes transactions with amounts >= threshold" $
        length (applyFilters Nothing Nothing [] Nothing [] [] (Just 1000) Nothing allTxns)
          `shouldBe` 2  -- txn2 (3000) and txn3 (1200)

      it "maxAmount includes transactions with any amount <= threshold" $
        length (applyFilters Nothing Nothing [] Nothing [] [] Nothing (Just 100) allTxns)
          `shouldBe` 3  -- all txns have at least one posting amount <= 100 (negative postings)

      it "minAmount and maxAmount together" $
        length (applyFilters Nothing Nothing [] Nothing [] [] (Just 100) (Just 2000) allTxns)
          `shouldBe` 2  -- txn2 (3000 >= 100, -3000 <= 2000) and txn3 (1200 >= 100, -1200 <= 2000)

    describe "combined filters" $ do
      it "no filters returns all transactions" $
        noFilter allTxns `shouldBe` allTxns

      it "status and date filter combine" $
        applyFilters (Just (fromGregorian 2025 2 1)) Nothing [] Nothing [H.Unmarked] [] Nothing Nothing allTxns
          `shouldBe` [txn2]

      it "multi-value account + single status combine with AND" $
        applyFilters Nothing Nothing ["expenses:food", "expenses:rent"] Nothing [H.Pending] [] Nothing Nothing allTxns
          `shouldBe` [txn3]

  describe "applySorting" $ do
    let mkTxn' date desc status amt = H.nulltransaction
          { H.tdate = date
          , H.tdescription = desc
          , H.tstatus = status
          , H.tpostings = [H.nullposting
              { H.pamount = H.mixed [H.nullamt { H.aquantity = amt, H.acommodity = "USD" }] }]
          }
        txnA = mkTxn' (fromGregorian 2025 1 1) "Alpha" H.Cleared 100
        txnB = mkTxn' (fromGregorian 2025 1 2) "Beta"  H.Unmarked 200
        txnC = mkTxn' (fromGregorian 2025 1 1) "Charlie" H.Pending 50

    it "sorts by date ascending (default)" $
      applySorting [SortSpec SortDate Asc] [txnB, txnA, txnC]
        `shouldSatisfy` (\ts -> map H.tdate ts == [fromGregorian 2025 1 1, fromGregorian 2025 1 1, fromGregorian 2025 1 2])

    it "sorts by date descending" $
      applySorting [SortSpec SortDate Desc] [txnA, txnB, txnC]
        `shouldSatisfy` (\ts -> map H.tdate ts == [fromGregorian 2025 1 2, fromGregorian 2025 1 1, fromGregorian 2025 1 1])

    it "multi-field sort: date asc then description asc" $
      let result = applySorting [SortSpec SortDate Asc, SortSpec SortDescription Asc] [txnB, txnC, txnA]
      in map H.tdescription result `shouldBe` ["Alpha", "Charlie", "Beta"]

    it "sorts by amount descending" $
      let result = applySorting [SortSpec SortAmount Desc] [txnA, txnB, txnC]
      in map H.tdescription result `shouldBe` ["Beta", "Alpha", "Charlie"]

    it "sorts by status ascending" $
      let result = applySorting [SortSpec SortStatus Asc] [txnA, txnB, txnC]
      in map H.tdescription result `shouldBe` ["Beta", "Charlie", "Alpha"]
