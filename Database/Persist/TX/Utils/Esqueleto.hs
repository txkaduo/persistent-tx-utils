module Database.Persist.TX.Utils.Esqueleto where

-- {{{1
import           ClassyPrelude                   hiding (delete)
import qualified Data.List.NonEmpty as LNE
import           Database.Persist
import qualified Database.Esqueleto              as E
import qualified Database.Esqueleto.Internal.Sql as E
import           Database.Persist.TX.Utils       (unsafeEscapeForSqlLikeT)

import Database.Persist.TX.Utils
-- }}}1


type EsqCondOf a = E.SqlExpr a -> E.SqlExpr (E.Value Bool)

type EsqCondOf2 a b = E.SqlExpr a -> E.SqlExpr b -> E.SqlExpr (E.Value Bool)

type EsqCondOfValue a = E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value Bool)

-- | 过滤某 Entity 表的函数
type EsqCondOfEntity a = EsqCondOf (Entity a)

type EsqCondOfEntity2 a b = EsqCondOf2 (Entity a) (Entity b)

-- | 过滤某 Entity 表的函数, Maybe 版
type EsqCondOfMaybeEntity a = EsqCondOf (Maybe (Entity a))


-- | Construct a ROW
esqPgSqlRow2 :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value (a, b))
esqPgSqlRow2 x1 x2 = E.unsafeSqlFunction "" (x1, x2)

esqPgSqlRow3 :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value c) -> E.SqlExpr (E.Value (a, b, c))
esqPgSqlRow3 x1 x2 x3 = E.unsafeSqlFunction "" (x1, x2, x3)

esqPgSqlRow4 :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value c) -> E.SqlExpr (E.Value d) -> E.SqlExpr (E.Value (a, b, c, d))
esqPgSqlRow4 x1 x2 x3 x4 = E.unsafeSqlFunction "" (x1, x2, x3, x4)


esqPgSqlArrayOverlap :: E.SqlExpr (E.Value [a]) -> E.SqlExpr (E.Value [b]) -> E.SqlExpr (E.Value Bool)
esqPgSqlArrayOverlap = E.unsafeSqlBinOp "&&"


esqOr :: LNE.NonEmpty (E.SqlExpr (E.Value Bool)) -> E.SqlExpr (E.Value Bool)
esqOr (x LNE.:| xs) = foldr (E.||.) x xs


esqAnd :: LNE.NonEmpty (E.SqlExpr (E.Value Bool)) -> E.SqlExpr (E.Value Bool)
esqAnd (x LNE.:| xs) = foldr (E.&&.) x xs



esqPgSqlContainsI :: E.SqlExpr (E.Value Text) -> Text -> E.SqlExpr (E.Value Bool)
esqPgSqlContainsI x t = x `E.ilike` E.concat_ [ (E.%), E.val (unsafeEscapeForSqlLikeT t), (E.%)]

esqPgSqlContainsMaybeI :: E.SqlExpr (E.Value (Maybe Text)) -> Text -> E.SqlExpr (E.Value Bool)
esqPgSqlContainsMaybeI x t = x `E.ilike` E.just (E.concat_ [ (E.%), E.val (unsafeEscapeForSqlLikeT t), (E.%)])

esqMySqlContainsI :: E.SqlExpr (E.Value Text) -> Text -> E.SqlExpr (E.Value Bool)
esqMySqlContainsI x t = x `E.like` E.concat_ [ (E.%), E.val (unsafeEscapeForSqlLikeT t), (E.%)]

esqMySqlContainsMaybeI :: E.SqlExpr (E.Value (Maybe Text)) -> Text -> E.SqlExpr (E.Value Bool)
esqMySqlContainsMaybeI x t = x `E.like` E.just (E.concat_ [ (E.%), E.val (unsafeEscapeForSqlLikeT t), (E.%)])


esqPgSqlContains :: E.SqlExpr (E.Value Text) -> Text -> E.SqlExpr (E.Value Bool)
esqPgSqlContains x t = x `E.like` E.concat_ [ (E.%), E.val (unsafeEscapeForSqlLikeT t), (E.%)]

esqPgSqlMaybeContains :: E.SqlExpr (E.Value (Maybe Text)) -> Text -> E.SqlExpr (E.Value Bool)
esqPgSqlMaybeContains x t = x `E.like` E.just (E.concat_ [ (E.%), E.val (unsafeEscapeForSqlLikeT t), (E.%)])

esqMySqlContains :: E.SqlExpr (E.Value Text) -> Text -> E.SqlExpr (E.Value Bool)
esqMySqlContains x t = x `esqMySqliLike` E.concat_ [ (E.%), E.val (unsafeEscapeForSqlLikeT t), (E.%)]

esqMySqlMaybeContains :: E.SqlExpr (E.Value (Maybe Text)) -> Text -> E.SqlExpr (E.Value Bool)
esqMySqlMaybeContains x t = x `esqMySqliLike` E.just (E.concat_ [ (E.%), E.val (unsafeEscapeForSqlLikeT t), (E.%)])

esqMySqliLike :: E.SqlString a
              => E.SqlExpr (E.Value a)
              -> E.SqlExpr (E.Value a)
              -> E.SqlExpr (E.Value Bool)
esqMySqliLike = E.unsafeSqlBinOp "LIKE BINARY"


esqJsonbContains :: E.SqlExpr (E.Value PersistJsonb)
                 -> E.SqlExpr (E.Value PersistJsonb)
                 -> E.SqlExpr (E.Value Bool)
esqJsonbContains = E.unsafeSqlBinOp "@>"


esqJsonbAtInt :: Integral i
              => E.SqlExpr (E.Value PersistJsonb)
              -> E.SqlExpr (E.Value i)
              -> E.SqlExpr (E.Value (Maybe PersistJsonb))
esqJsonbAtInt = E.unsafeSqlBinOp "->"


esqJsonbAtIntMaybe :: Integral i
                   => E.SqlExpr (E.Value (Maybe PersistJsonb))
                   -> E.SqlExpr (E.Value i)
                   -> E.SqlExpr (E.Value (Maybe PersistJsonb))
esqJsonbAtIntMaybe = E.unsafeSqlBinOp "->"


esqJsonAtInt :: Integral i
             => E.SqlExpr (E.Value PersistJson)
             -> E.SqlExpr (E.Value i)
             -> E.SqlExpr (E.Value (Maybe PersistJson))
esqJsonAtInt = E.unsafeSqlBinOp "->"


esqJsonAtIntMaybe :: Integral i
                  => E.SqlExpr (E.Value (Maybe PersistJson))
                  -> E.SqlExpr (E.Value i)
                  -> E.SqlExpr (E.Value (Maybe PersistJson))
esqJsonAtIntMaybe = E.unsafeSqlBinOp "->"


esqJsonbAtKey :: E.SqlExpr (E.Value PersistJsonb)
              -> E.SqlExpr (E.Value Text)
              -> E.SqlExpr (E.Value (Maybe PersistJsonb))
esqJsonbAtKey = E.unsafeSqlBinOp "->"


esqJsonbAtKeyMaybe :: E.SqlExpr (E.Value (Maybe PersistJsonb))
                   -> E.SqlExpr (E.Value Text)
                   -> E.SqlExpr (E.Value (Maybe PersistJsonb))
esqJsonbAtKeyMaybe = E.unsafeSqlBinOp "->"


esqJsonAtKey :: E.SqlExpr (E.Value PersistJson)
             -> E.SqlExpr (E.Value Text)
             -> E.SqlExpr (E.Value (Maybe PersistJson))
esqJsonAtKey = E.unsafeSqlBinOp "->"


esqJsonAtKeyMaybe :: E.SqlExpr (E.Value (Maybe PersistJson))
                  -> E.SqlExpr (E.Value Text)
                  -> E.SqlExpr (E.Value (Maybe PersistJson))
esqJsonAtKeyMaybe = E.unsafeSqlBinOp "->"


esqJsonbAtIntText :: Integral i
                  => E.SqlExpr (E.Value PersistJsonb)
                  -> E.SqlExpr (E.Value i)
                  -> E.SqlExpr (E.Value (Maybe Text))
esqJsonbAtIntText = E.unsafeSqlBinOp "->>"


esqJsonbAtIntTextMaybe :: Integral i
                       => E.SqlExpr (E.Value (Maybe PersistJsonb))
                       -> E.SqlExpr (E.Value i)
                       -> E.SqlExpr (E.Value (Maybe Text))
esqJsonbAtIntTextMaybe = E.unsafeSqlBinOp "->>"


esqJsonAtIntText :: Integral i
                 => E.SqlExpr (E.Value PersistJson)
                 -> E.SqlExpr (E.Value i)
                 -> E.SqlExpr (E.Value (Maybe Text))
esqJsonAtIntText = E.unsafeSqlBinOp "->>"


esqJsonAtIntTextMaybe :: Integral i
                      => E.SqlExpr (E.Value (Maybe PersistJson))
                      -> E.SqlExpr (E.Value i)
                      -> E.SqlExpr (E.Value (Maybe Text))
esqJsonAtIntTextMaybe = E.unsafeSqlBinOp "->>"


esqJsonbAtKeyText :: E.SqlExpr (E.Value PersistJsonb)
                  -> E.SqlExpr (E.Value Text)
                  -> E.SqlExpr (E.Value (Maybe Text))
esqJsonbAtKeyText = E.unsafeSqlBinOp "->>"


esqJsonbAtKeyTextMaybe :: E.SqlExpr (E.Value (Maybe PersistJsonb))
                       -> E.SqlExpr (E.Value Text)
                       -> E.SqlExpr (E.Value (Maybe Text))
esqJsonbAtKeyTextMaybe = E.unsafeSqlBinOp "->>"


esqJsonAtKeyText :: E.SqlExpr (E.Value PersistJson)
                 -> E.SqlExpr (E.Value Text)
                 -> E.SqlExpr (E.Value (Maybe Text))
esqJsonAtKeyText = E.unsafeSqlBinOp "->>"


esqJsonAtKeyTextMaybe :: E.SqlExpr (E.Value (Maybe PersistJson))
                      -> E.SqlExpr (E.Value Text)
                      -> E.SqlExpr (E.Value (Maybe Text))
esqJsonAtKeyTextMaybe = E.unsafeSqlBinOp "->>"



-- vim: set foldmethod=marker:
