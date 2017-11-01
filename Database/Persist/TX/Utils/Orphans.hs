{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.TX.Utils.Orphans where

import ClassyPrelude
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

