{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.TX.Utils.Orphans where

import ClassyPrelude
import Data.Aeson as A
import Data.Proxy
import Web.PathPieces
import Data.UUID as U
import Database.Persist.Sql
import Database.PostgreSQL.Simple.Time (Unbounded(..))


#if !MIN_VERSION_persistent(2, 10, 5)
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
#endif

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "UUID"

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . U.toASCIIBytes

  fromPersistValue (PersistByteString bs) = maybe (Left "Invalid UUID text") Right $ U.fromASCIIBytes bs
  fromPersistValue (PersistText t)        = maybe (Left "Invalid UUID text") Right $ U.fromText t
#if MIN_VERSION_persistent(2, 9, 0)
  fromPersistValue (PersistLiteralEscaped bs) = maybe (Left "Invalid UUID ByteString") Right $ U.fromASCIIBytes bs
  fromPersistValue (PersistLiteral bs)        = maybe (Left "Invalid UUID ByteString") Right $ U.fromASCIIBytes bs
#else
  fromPersistValue (PersistDbSpecific bs)    = maybe (Left "Invalid UUID ByteString") Right $ U.fromASCIIBytes bs
#endif
  fromPersistValue x                      = Left $ "PersistJson must be converted from PersistDbSpecific/PersistByteString/PersistText, but got " <> tshow x

instance PathPiece UUID where
  toPathPiece = U.toText
  fromPathPiece = U.fromText


instance PersistField a => PersistField (Unbounded a) where
  toPersistValue PosInfinity = PersistText "infinity"
  toPersistValue NegInfinity = PersistText "-infinity"
  toPersistValue (Finite x)  = toPersistValue x

  fromPersistValue (PersistText "infinity")        = pure PosInfinity
  fromPersistValue (PersistText "-infinity")       = pure NegInfinity
  fromPersistValue (PersistDbSpecific "infinity")  = pure PosInfinity
  fromPersistValue (PersistDbSpecific "-infinity") = pure NegInfinity
  fromPersistValue (PersistByteString "infinity")  = pure PosInfinity
  fromPersistValue (PersistByteString "-infinity") = pure NegInfinity
  fromPersistValue x                               = fmap Finite $ fromPersistValue x

instance PersistFieldSql a => PersistFieldSql (Unbounded a) where
  sqlType _ = sqlType (Proxy :: Proxy a)


instance FromJSON a => FromJSON (Unbounded a) where
  parseJSON (A.String "infinity")  = pure PosInfinity
  parseJSON (A.String "-infinity") = pure NegInfinity
  parseJSON x                      = fmap Finite $ parseJSON x

instance ToJSON a => ToJSON (Unbounded a) where
  toJSON PosInfinity = A.String "infinity"
  toJSON NegInfinity = A.String "-infinity"
  toJSON (Finite x)  = toJSON x
