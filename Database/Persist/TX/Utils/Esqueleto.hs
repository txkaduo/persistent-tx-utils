module Database.Persist.TX.Utils.Esqueleto where

-- {{{1
import           ClassyPrelude                   hiding (delete)
import           Control.Exception (throw)
import qualified Data.List.NonEmpty as LNE
import           Database.Persist
import qualified Database.Esqueleto              as E
import qualified Database.Esqueleto.Internal.Sql as E
import           Database.Persist.TX.Utils       (unsafeEscapeForSqlLikeT)
import qualified Data.Text.Lazy.Builder          as TLB

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


esqUnValue2 :: (E.Value a, E.Value b) -> (a, b)
esqUnValue2 = E.unValue *** E.unValue


esqUnValue3 :: (E.Value a, E.Value b, E.Value c) -> (a, b, c)
esqUnValue3 (E.Value a, E.Value b, E.Value c) = (a, b, c)


esqUnValue4 :: (E.Value a, E.Value b, E.Value c, E.Value d) -> (a, b, c, d)
esqUnValue4 (E.Value a, E.Value b, E.Value c, E.Value d) = (a, b, c, d)


esqUnValue5 :: (E.Value a, E.Value b, E.Value c, E.Value d, E.Value e) -> (a, b, c, d, e)
esqUnValue5 (E.Value a, E.Value b, E.Value c, E.Value d, E.Value e) = (a, b, c, d, e)


esqPgSqlUTCTimeToDayMaybe :: E.SqlExpr (E.Value (Maybe UTCTime)) -> E.SqlExpr (E.Value (Maybe Day))
esqPgSqlUTCTimeToDayMaybe = E.unsafeSqlFunction "DATE"

esqPgSqlUTCTimeToDay :: E.SqlExpr (E.Value UTCTime) -> E.SqlExpr (E.Value Day)
esqPgSqlUTCTimeToDay = E.unsafeSqlFunction "DATE"


-- | Construct a ROW in PostgreSQL
esqPgSqlRow2 :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value (a, b))
esqPgSqlRow2 x1 x2 = E.unsafeSqlFunction "" (x1, x2)

esqPgSqlRow3 :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value c) -> E.SqlExpr (E.Value (a, b, c))
esqPgSqlRow3 x1 x2 x3 = E.unsafeSqlFunction "" (x1, x2, x3)

esqPgSqlRow4 :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value c) -> E.SqlExpr (E.Value d) -> E.SqlExpr (E.Value (a, b, c, d))
esqPgSqlRow4 x1 x2 x3 x4 = E.unsafeSqlFunction "" (x1, x2, x3, x4)


-- | ARRAY[?, ?, ?]
#if MIN_VERSION_esqueleto(3, 0, 0)
esqPgSqlArrayVal :: PersistField a => [a] -> E.SqlExpr (E.Value [a])
esqPgSqlArrayVal vals = E.ERaw E.Never $ const $
  ("ARRAY[" <> uncommas ("?" <$ vals) <> "]", map toPersistValue vals)
#else
-- This should be: esqPgSqlArrayVal :: PersistField a => [a] -> E.SqlExpr (E.Value [a])
-- But don't know how to implement it for all PersistField types
esqPgSqlArrayVal :: ToBackendKey SqlBackend a => [Key a] -> E.SqlExpr (E.Value [Key a])
esqPgSqlArrayVal xs = E.unsafeSqlValue $ TLB.fromText "ARRAY[" <> args <> "]"
  where args = TLB.fromText $ intercalate ", " $ map (tshow . fromSqlKey) xs
#endif


#if MIN_VERSION_esqueleto(3, 0, 0)
esqPgSqlArray :: [E.SqlExpr (E.Value a)] -> E.SqlExpr (E.Value [a])
esqPgSqlArray args =
  E.ERaw E.Never $ \info ->
    let (argsTLB, argsVals) =
          uncommas' $ map (\(E.ERaw _ f) -> f info) $ E.toArgList args
    in ("ARRAY[" <> argsTLB <> "]", argsVals)


-- | CAST (expression AS type)
esqUnsafeCastAs :: Text -> E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b)
esqUnsafeCastAs t (E.ERaw p f) = E.ERaw E.Never $
                                \ info -> let (b, vals) = f info
                                           in ("CAST (" <> parensM p b <> " AS " <> TLB.fromText t <> ")", vals)
esqUnsafeCastAs _ (E.ECompositeKey _) = throw (userError "cannot 'cast as' on ECompositeKey")

esqList :: [E.SqlExpr (E.Value a)] -> E.SqlExpr (E.ValueList a)
esqList [] = E.EEmptyList
esqList args =
  E.EList $ E.ERaw E.Never $ \info ->
    let (argsTLB, argsVals) =
          uncommas' $ map (\(E.ERaw _ f) -> f info) $ E.toArgList args
    in ("(" <> argsTLB <> ")", argsVals)
#endif


esqPgSqlArrayOverlap :: E.SqlExpr (E.Value [a]) -> E.SqlExpr (E.Value [a]) -> E.SqlExpr (E.Value Bool)
esqPgSqlArrayOverlap = E.unsafeSqlBinOp " && "

esqPgSqlArrayOverlapMay :: E.SqlExpr (E.Value (Maybe [a])) -> E.SqlExpr (E.Value (Maybe [a])) -> E.SqlExpr (E.Value Bool)
esqPgSqlArrayOverlapMay = E.unsafeSqlBinOp " && "


esqPgSqlArrayAny :: E.SqlExpr (E.Value [a]) -> E.SqlExpr (E.Value a)
esqPgSqlArrayAny = E.unsafeSqlFunction "ANY"


esqPgSqlArrayAnyMay :: E.SqlExpr (E.Value (Maybe [a])) -> E.SqlExpr (E.Value (Maybe a))
esqPgSqlArrayAnyMay = E.unsafeSqlFunction "ANY"


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


-- Some code copied from Esqueleto
uncommas :: [TLB.Builder] -> TLB.Builder
uncommas = intersperseB ", "

intersperseB :: TLB.Builder -> [TLB.Builder] -> TLB.Builder
intersperseB a = mconcat . intersperse a . filter (/= mempty)

uncommas' :: Monoid a => [(TLB.Builder, a)] -> (TLB.Builder, a)
uncommas' = (uncommas *** mconcat) . unzip

parens :: TLB.Builder -> TLB.Builder
parens b = "(" <> (b <> ")")

parensM :: E.NeedParens -> TLB.Builder -> TLB.Builder
parensM E.Never  = id
parensM E.Parens = parens

between :: E.PersistField typ
        => E.SqlExpr (E.Value typ)
        -> (E.SqlExpr (E.Value typ), E.SqlExpr (E.Value typ))
        -> E.SqlExpr (E.Value Bool)
between x (a, b) = x E.>=. a E.&&. x E.<. b

-- vim: set foldmethod=marker:
