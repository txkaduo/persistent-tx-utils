{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.TX.Utils.Orphans where

import ClassyPrelude
import Web.PathPieces
import Data.UUID as U
import Database.Persist.Sql


instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e, RawSql f,
          RawSql g, RawSql h, RawSql i)
      => RawSql (a, b, c, d, e, f, g, h, i) where
    rawSqlCols e         = rawSqlCols e         . from9
    rawSqlColCountReason = rawSqlColCountReason . from9
    rawSqlProcessRow     = fmap to9 . rawSqlProcessRow

from9 :: (a,b,c,d,e,f,g,h,i) -> ((a,b),(c,d),(e,f),(g,h),i)
from9 (a,b,c,d,e,f,g,h,i) = ((a,b),(c,d),(e,f),(g,h),i)

to9 :: ((a,b),(c,d),(e,f),(g,h),i) -> (a,b,c,d,e,f,g,h,i)
to9 ((a,b),(c,d),(e,f),(g,h),i) = (a,b,c,d,e,f,g,h,i)


instance PersistFieldSql UUID where
  sqlType _ = SqlOther "UUID"

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . U.toASCIIBytes

  fromPersistValue (PersistDbSpecific bs) = maybe (Left "Invalid UUID text") Right $ U.fromASCIIBytes bs
  fromPersistValue (PersistByteString bs) = maybe (Left "Invalid UUID text") Right $ U.fromASCIIBytes bs
  fromPersistValue (PersistText t)        = maybe (Left "Invalid UUID text") Right $ U.fromText t
  fromPersistValue x                      = Left $ "PersistJson must be converted from PersistDbSpecific/PersistByteString/PersistText, but got " <> tshow x

instance PathPiece UUID where
  toPathPiece = U.toText
  fromPathPiece = U.fromText
