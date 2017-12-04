module Database.Persist.TX.Utils.Esqueleto where

-- {{{1
import           ClassyPrelude                   hiding (delete)
import qualified Database.Esqueleto              as E
import qualified Database.Esqueleto.Internal.Sql as E
import           Database.Persist.TX.Utils       (unsafeEscapeForSqlLikeT)
-- }}}1


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

-- vim: set foldmethod=marker:
