module Api.Types.Sort
  ( SortField(..)
  , SortDirection(..)
  , SortSpec(..)
  , parseSortParam
  ) where

import Data.Text (Text)

import qualified Data.Text as T

data SortField = SortDate | SortDescription | SortStatus | SortAmount
  deriving (Show, Eq)

data SortDirection = Asc | Desc
  deriving (Show, Eq)

data SortSpec = SortSpec
  { sortField     :: SortField
  , sortDirection :: SortDirection
  } deriving (Show, Eq)

parseSortParam :: Text -> Either Text [SortSpec]
parseSortParam t = traverse parseOne (T.splitOn "," t)
  where
    parseOne s =
      let (prefix, name) = case T.uncons s of
            Just ('-', rest) -> (Desc, rest)
            _                -> (Asc, s)
      in case T.toLower name of
        "date"        -> Right (SortSpec SortDate prefix)
        "description" -> Right (SortSpec SortDescription prefix)
        "status"      -> Right (SortSpec SortStatus prefix)
        "amount"      -> Right (SortSpec SortAmount prefix)
        _             -> Left ("Unknown sort field: " <> name)
