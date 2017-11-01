module Database.Persist.TX.Utils.Dummy where


import           Database.Persist.Sql
import           Database.Persist.TH


share [mkPersist sqlSettings]
  [persistLowerCase|
-- | XXX: 这个仅仅是为了 esqueleto 代码而做的hack
-- 只是haskell类型上的需要，表数据安全没关系
-- 下面有表用 RawEnt 代表一种任意表的id，即代替写 Int64
-- to make ToBaseId instances
Dummy

  |]
