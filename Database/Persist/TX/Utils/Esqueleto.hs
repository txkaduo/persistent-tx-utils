module Database.Persist.TX.Utils.Esqueleto where

-- {{{1
import           ClassyPrelude                   hiding (delete)
import qualified Data.List.NonEmpty as LNE
import qualified Data.Text.Lazy.Builder          as TLB
import           Database.Persist

#if MIN_VERSION_esqueleto(3, 5, 0)
import qualified Database.Esqueleto.Legacy       as E
#else
import           Control.Exception (throw)
import qualified Database.Esqueleto              as E
#endif

#if MIN_VERSION_esqueleto(3, 4, 0)
import qualified Database.Esqueleto.Internal.Internal as E
#else
import qualified Database.Esqueleto.Internal.Sql as E
import qualified Database.Esqueleto.Internal.Language as E
#endif

import           Database.PostgreSQL.Simple.Time (Unbounded(..), Date)

import           GHC.TypeLits

import Database.Persist.TX.Utils
-- }}}1

type EsqExprValue a = E.SqlExpr (E.Value a)

type EsqExprEntity a = E.SqlExpr (Entity a)

type EsqExprMaybeEntity a = E.SqlExpr (Maybe (Entity a))

type EsqCondOf a = E.SqlExpr a -> EsqExprValue Bool

type EsqCondOf2 a b = E.SqlExpr a -> E.SqlExpr b -> EsqExprValue Bool

type EsqCondOf3 a b c = E.SqlExpr a -> E.SqlExpr b -> E.SqlExpr c -> EsqExprValue Bool

type EsqCondOfValue a = EsqExprValue a -> EsqExprValue Bool

-- | 过滤某 Entity 表的函数
type EsqCondOfEntity a = EsqCondOf (Entity a)

type EsqCondOfEntity2 a b = EsqCondOf2 (Entity a) (Entity b)

-- | 过滤某 Entity 表的函数, Maybe 版
type EsqCondOfMaybeEntity a = EsqCondOf (Maybe (Entity a))


-- | 可以用作 E.from 第一个参数的类型条件
type EsqCanSelectFrom a = (E.FromPreprocess a, E.From a)


esqPagerQuery :: Int -> Int -> E.SqlQuery ()
esqPagerQuery npp pn = do
  E.offset $ fromIntegral $ (pn - 1) * npp
  E.limit $ fromIntegral npp


esqUnsafeFromSqlKey :: E.SqlExpr (E.Value (Key ent)) -> E.SqlExpr (E.Value Int64)
esqUnsafeFromSqlKey = E.veryUnsafeCoerceSqlExprValue


esqUnsafeFromSqlKeyMaybe :: E.SqlExpr (E.Value (Maybe (Key ent))) -> E.SqlExpr (E.Value (Maybe Int64))
esqUnsafeFromSqlKeyMaybe = E.veryUnsafeCoerceSqlExprValue


esqIsJust :: (PersistField a) => E.SqlExpr (E.Value (Maybe a)) -> E.SqlExpr (E.Value Bool)
esqIsJust = E.not_ . E.isNothing


esqEqMaybe :: (PersistField a) => E.SqlExpr (E.Value (Maybe a)) -> Maybe a -> E.SqlExpr (E.Value Bool)
esqEqMaybe f Nothing = E.isNothing f
esqEqMaybe f (Just x) = f E.==. E.val (Just x)


esqValueAsInt :: E.SqlExpr (E.Value Int) -> E.SqlExpr (E.Value Int)
esqValueAsInt = id

esqValueAsIntMaybe :: E.SqlExpr (E.Value (Maybe Int)) -> E.SqlExpr (E.Value (Maybe Int))
esqValueAsIntMaybe = id

esqValueAsInt64 :: E.SqlExpr (E.Value Int64) -> E.SqlExpr (E.Value Int64)
esqValueAsInt64 = id

esqValueAsInt64Maybe :: E.SqlExpr (E.Value (Maybe Int64)) -> E.SqlExpr (E.Value (Maybe Int64))
esqValueAsInt64Maybe = id

esqValueAsDouble :: E.SqlExpr (E.Value Double) -> E.SqlExpr (E.Value Double)
esqValueAsDouble = id

esqValueAsDoubleMaybe :: E.SqlExpr (E.Value (Maybe Double)) -> E.SqlExpr (E.Value (Maybe Double))
esqValueAsDoubleMaybe = id

esqUnValue2 :: (E.Value a, E.Value b) -> (a, b)
esqUnValue2 = E.unValue *** E.unValue


esqUnValue3 :: (E.Value a, E.Value b, E.Value c) -> (a, b, c)
esqUnValue3 (E.Value a, E.Value b, E.Value c) = (a, b, c)


esqUnValue4 :: (E.Value a, E.Value b, E.Value c, E.Value d) -> (a, b, c, d)
esqUnValue4 (E.Value a, E.Value b, E.Value c, E.Value d) = (a, b, c, d)


esqUnValue5 :: (E.Value a, E.Value b, E.Value c, E.Value d, E.Value e) -> (a, b, c, d, e)
esqUnValue5 (E.Value a, E.Value b, E.Value c, E.Value d, E.Value e) = (a, b, c, d, e)

esqUnValue6 :: (E.Value a, E.Value b, E.Value c, E.Value d, E.Value e, E.Value f) -> (a, b, c, d, e, f)
esqUnValue6 (E.Value a, E.Value b, E.Value c, E.Value d, E.Value e, E.Value f) = (a, b, c, d, e, f)


class EsqHasDayPart a

instance EsqHasDayPart UTCTime

instance EsqHasDayPart Day

instance EsqHasDayPart Date

type family DatePartResult (a :: Symbol) where
  DatePartResult "week"  = Int
  DatePartResult "month" = Int
  DatePartResult "year"  = Int64  -- ^ cannot use Integer, because Integer is not a PersistField


esqPgSqlToDayMaybe :: EsqHasDayPart a => E.SqlExpr (E.Value (Maybe a)) -> E.SqlExpr (E.Value (Maybe Day))
esqPgSqlToDayMaybe = E.unsafeSqlFunction "DATE"

esqPgSqlToDay :: EsqHasDayPart a => E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value Day)
esqPgSqlToDay = E.unsafeSqlFunction "DATE"


esqPgSqlDatePartMaybe :: (EsqHasDayPart a, KnownSymbol s)
                      => proxy s
                      -> E.SqlExpr (E.Value (Maybe a))
                      -> E.SqlExpr (E.Value (Maybe (DatePartResult s)))
esqPgSqlDatePartMaybe x1 x2 = E.unsafeSqlFunction "DATE_PART" (E.val (symbolVal x1), x2)

esqPgSqlDatePart :: (EsqHasDayPart a, KnownSymbol s)
                 => proxy s
                 -> E.SqlExpr (E.Value a)
                 -> E.SqlExpr (E.Value (DatePartResult s))
esqPgSqlDatePart x1 x2 = E.unsafeSqlFunction "DATE_PART" (E.val (symbolVal x1), x2)


esqPgSqlDayAdd :: Integral a => E.SqlExpr (E.Value Day) -> E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value Day)
esqPgSqlDayAdd = E.unsafeSqlBinOp " + "

esqPgSqlDayMaybeAdd :: Integral a => E.SqlExpr (E.Value (Maybe Day)) -> E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value (Maybe Day))
esqPgSqlDayMaybeAdd = E.unsafeSqlBinOp " + "

esqPgSqlDayMinus :: Integral a => E.SqlExpr (E.Value Day) -> E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value Day)
esqPgSqlDayMinus = E.unsafeSqlBinOp " - "

esqPgSqlDayMaybeMinus :: Integral a => E.SqlExpr (E.Value (Maybe Day)) -> E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value (Maybe Day))
esqPgSqlDayMaybeMinus = E.unsafeSqlBinOp " - "


esqPgSqlDayToUnbounded :: E.SqlExpr (E.Value Day) -> E.SqlExpr (E.Value (Unbounded Day))
esqPgSqlDayToUnbounded = E.veryUnsafeCoerceSqlExprValue

esqPgSqlDayToUnboundedMaybe :: E.SqlExpr (E.Value (Maybe Day)) -> E.SqlExpr (E.Value (Maybe (Unbounded Day)))
esqPgSqlDayToUnboundedMaybe = E.veryUnsafeCoerceSqlExprValue


esqConcat :: E.UnsafeSqlFunctionArgument a => a -> E.SqlExpr (E.Value Text)
esqConcat = E.unsafeSqlFunction "CONCAT"


-- | Construct a ROW in PostgreSQL
esqPgSqlRow2 :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value (a, b))
esqPgSqlRow2 x1 x2 = E.unsafeSqlFunction "" (x1, x2)

esqPgSqlRow3 :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value c) -> E.SqlExpr (E.Value (a, b, c))
esqPgSqlRow3 x1 x2 x3 = E.unsafeSqlFunction "" (x1, x2, x3)

esqPgSqlRow4 :: E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value c) -> E.SqlExpr (E.Value d) -> E.SqlExpr (E.Value (a, b, c, d))
esqPgSqlRow4 x1 x2 x3 x4 = E.unsafeSqlFunction "" (x1, x2, x3, x4)


-- | ARRAY[?, ?, ?]
esqPgSqlArrayVal :: PersistField a => [a] -> E.SqlExpr (E.Value [a])
#if MIN_VERSION_esqueleto(3, 5, 0)
esqPgSqlArrayVal vals = E.ERaw E.noMeta $ \ _need_parens _info ->
                          ("ARRAY[" <> uncommas ("?" <$ vals) <> "]", map toPersistValue vals)
#elif MIN_VERSION_esqueleto(3, 0, 0)
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
#if MIN_VERSION_esqueleto(3, 5, 0)
  E.ERaw E.noMeta $ \ need_parens info ->
    let (argsTLB, argsVals) =
          uncommas' $ map (\(E.ERaw _ f) -> f need_parens info) $ E.toArgList args
    in ("ARRAY[" <> argsTLB <> "]", argsVals)
#else
  E.ERaw E.Never $ \info ->
    let (argsTLB, argsVals) =
          uncommas' $ map (\(E.ERaw _ f) -> f info) $ E.toArgList args
    in ("ARRAY[" <> argsTLB <> "]", argsVals)
#endif


-- | CAST (expression AS type)
esqUnsafeCastAs :: Text -> E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b)
#if MIN_VERSION_esqueleto(3, 5, 0)
esqUnsafeCastAs t (E.ERaw meta f) =
                            E.ERaw meta $
                                \ need_parens info -> let (b, vals) = f need_parens info
                                           in ("CAST (" <> parensM need_parens b <> " AS " <> TLB.fromText t <> ")", vals)
#else
esqUnsafeCastAs t (E.ERaw p f) =
                            E.ERaw E.Never $
                                \ info -> let (b, vals) = f info
                                           in ("CAST (" <> parensM p b <> " AS " <> TLB.fromText t <> ")", vals)
esqUnsafeCastAs _ (E.ECompositeKey _) = throw (userError "cannot 'cast as' on ECompositeKey")

#if MIN_VERSION_esqueleto(3, 3, 0)
esqUnsafeCastAs _ (E.EAliasedValue _ _) = throw (userError "cannot 'cast as' on EAliasedValue")
esqUnsafeCastAs _ (E.EValueReference _ _) = throw (userError "cannot 'cast as' on EValueReference")
#endif

#endif



-- ARRAY [] 这样的式子会被理解为 int[] ，与 bigint[] 不匹配
esqPgSqlBigIntArrayVal :: ToBackendKey E.SqlBackend a => [Key a] -> E.SqlExpr (E.Value [Key a])
esqPgSqlBigIntArrayVal = E.veryUnsafeCoerceSqlExprValue . esqUnsafeCastAs "BIGINT[]" . esqPgSqlArray . map E.val

esqPgSqlBigIntArrayValMay :: ToBackendKey E.SqlBackend a => [Maybe (Key a)] -> E.SqlExpr (E.Value [Maybe (Key a)])
esqPgSqlBigIntArrayValMay = E.veryUnsafeCoerceSqlExprValue . esqUnsafeCastAs "BIGINT[]" . esqPgSqlArray . map E.val


esqList :: [E.SqlExpr (E.Value a)] -> E.SqlExpr (E.ValueList a)
#if MIN_VERSION_esqueleto(3, 5, 0)
esqList args = E.ERaw E.noMeta $ \ need_parens info ->
                let (argsTLB, argsVals) =
                      uncommas' $ map (\(E.ERaw _ f) -> f need_parens info) $ E.toArgList args
                in ("(" <> argsTLB <> ")", argsVals)
#else
esqList [] = E.EEmptyList
esqList args =
  E.EList $ E.ERaw E.Never $ \info ->
    let (argsTLB, argsVals) =
          uncommas' $ map (\(E.ERaw _ f) -> f info) $ E.toArgList args
    in ("(" <> argsTLB <> ")", argsVals)
#endif

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


-- | 某种可以做SQL过滤条件的规则
-- ts 通常是数据库表或其集合
-- 各条件之间是 逻辑或 的关系
newtype EsqLogicOr ts = EsqLogicOr ( LNE.NonEmpty ( ts -> EsqExprValue Bool ) )
  deriving (Semigroup)

runEsqLogicOr :: EsqLogicOr ts -> ts -> EsqExprValue Bool
runEsqLogicOr (EsqLogicOr conds) ts = esqOr $ map ($ ts) conds


-- | 某种可以做SQL过滤条件的规则
-- ts 通常是数据库表或其集合
-- 各条件之间是 逻辑或 的关系
newtype EsqLogicAnd ts = EsqLogicAnd ( LNE.NonEmpty ( ts -> EsqExprValue Bool ) )
  deriving (Semigroup)

runEsqLogicAnd :: EsqLogicAnd ts -> ts -> EsqExprValue Bool
runEsqLogicAnd (EsqLogicAnd conds) ts = esqAnd $ map ($ ts) conds


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


esqBetweenOpen :: (Ord typ,  E.PersistField typ)
               => E.SqlExpr (E.Value typ)
               -> (E.SqlExpr (E.Value typ), E.SqlExpr (E.Value typ))
               -> E.SqlExpr (E.Value Bool)
esqBetweenOpen x (a, b) = x E.>. a E.&&. x E.<. b


esqBetweenLeftOpen :: (Ord typ,  E.PersistField typ)
                    => E.SqlExpr (E.Value typ)
                    -> (E.SqlExpr (E.Value typ), E.SqlExpr (E.Value typ))
                    -> E.SqlExpr (E.Value Bool)
esqBetweenLeftOpen x (a, b) = x E.>. a E.&&. x E.<=. b


esqBetweenRightOpen :: (Ord typ,  E.PersistField typ)
                    => E.SqlExpr (E.Value typ)
                    -> (E.SqlExpr (E.Value typ), E.SqlExpr (E.Value typ))
                    -> E.SqlExpr (E.Value Bool)
esqBetweenRightOpen x (a, b) = x E.>=. a E.&&. x E.<. b


esqBetweenClose :: (Ord typ,  E.PersistField typ)
                => E.SqlExpr (E.Value typ)
                -> (E.SqlExpr (E.Value typ), E.SqlExpr (E.Value typ))
                -> E.SqlExpr (E.Value Bool)
esqBetweenClose x (a, b) = x E.>=. a E.&&. x E.<=. b



-- | 两次 SQL 查询，一次得到结果列表，另一次得到总数
esqGetResultsAndTotalNum :: (E.SqlSelect a r, E.From t, MonadIO m)
                         => (t -> E.SqlQuery a) -- ^ 过滤条件
                         -> (t -> [E.SqlExpr E.OrderBy])  -- ^ 排序
                         -> Int64                  -- ^ num per page
                         -> Int64                  -- ^ page num (1-based)
                         -> E.SqlReadT m ([r], Int64)  -- ^ result list, total number
esqGetResultsAndTotalNum filter_record mk_orders npp pn = do
  records <- E.select $ E.from $ \ t -> do
    v <- filter_record t

    let orders = mk_orders t
    unless (null orders) $ E.orderBy orders

    E.limit npp
    E.offset $ max 0 $ npp * (pn - 1)
    pure v

  cnt <- fmap (fromMaybe 0 . listToMaybe . map E.unValue) $ E.select $ E.from $ \ t -> do
          void $ filter_record t
          return E.countRows

  pure (records, cnt)



-- vim: set foldmethod=marker:
