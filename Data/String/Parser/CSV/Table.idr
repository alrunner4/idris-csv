module Data.String.Parser.CSV.Table
import Data.HVect
import Data.List
import Data.Maybe
import Data.SortedMap.Dependent
import Data.String.Parser
import Data.String.Parser.CSV
import Data.Vect
import Decidable.Equality

import Control.Monad.Identity

public export
ColumnTyper: Type
ColumnTyper = String -> Type

public export
ColumnParser: (Type -> Type) -> ColumnTyper -> Type
ColumnParser m t = (s: String) -> ParseT m (t s)

public export
TableRecord: ColumnTyper -> Type
TableRecord t = (s: String) -> Either String (t s)

public export
Table: ColumnTyper -> Type
Table t = List (TableRecord t)

record ColumnHeader (m: Type -> Type) (t: ColumnTyper) where
  constructor MkColumnHeader
  name: String
  parser: ParseT m (t name)


export
parseTable: Monad m => (t: ColumnTyper) -> ColumnParser m t -> List1 CSVRecord -> m (Table t)
parseTable t columnParser (header ::: rs) = parseRecords where

  typeHeaders: List1 (ColumnHeader m t)
  typeHeaders = header <&> \col => MkColumnHeader col (columnParser col)

  parseField: (String, ColumnHeader m t) -> m (x ** Either String (t x))
  parseField (s, h) = case !(parseT h.parser s) of
    Right (val, _) => pure (h.name ** Right val)
    Left   err     => pure (h.name ** Left  err)

  parseRecord: CSVRecord -> m (SortedDMap String (\a => Either String (t a)))
  parseRecord raw = fromList . forget <$> (traverse parseField (zip raw typeHeaders))

  parseRecords: m (Table t)
  parseRecords = for rs $ \r => do
    pr <- parseRecord r
    pure$ \col => case lookupPrecise col pr of
      Nothing          => Left "unknown column: \"\{col}\""
      Just (Left  err) => Left "parse failure: \{err}"
      Just (Right val) => Right val

