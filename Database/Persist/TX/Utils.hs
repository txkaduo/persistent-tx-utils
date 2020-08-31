module Database.Persist.TX.Utils where

-- {{{1
import ClassyPrelude                        hiding (delete)
#if !MIN_VERSION_classy_prelude(1, 5, 0)
import Control.DeepSeq                      (NFData(..), deepseq)
#endif
import Data.Aeson                           (Value, ToJSON(..), FromJSON(..))
import Data.ByteString.Builder              (toLazyByteString)
import Data.Char
import Data.Conduit
import Data.List (dropWhileEnd)
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.PostgreSQL.Simple.Time
import System.FilePath

import Control.Monad.Except                 (ExceptT, MonadError(..))

import qualified Data.Aeson                 as A
import qualified Data.List                  as L

import Control.Monad.State.Strict           (StateT)
import qualified Control.Monad.State.Strict as S

import Language.Haskell.TH
-- }}}1


type SqlWriteSource m a = forall backend. SqlBackendCanWrite backend =>
#if MIN_VERSION_conduit(1, 3, 0)
  ConduitT () a (ReaderT backend m) ()
#else
  Source (ReaderT backend m) a
#endif


type SqlWriteConduit i m o = forall backend. SqlBackendCanWrite backend =>
#if MIN_VERSION_conduit(1, 3, 0)
  ConduitT i o (ReaderT backend m) ()
#else
  Conduit i (ReaderT backend m) o
#endif


type SqlWriteSink i m r = forall backend. SqlBackendCanWrite backend =>
#if MIN_VERSION_conduit(1, 3, 0)
  ConduitT i Void (ReaderT backend m) r
#else
  Sink i (ReaderT backend m) r
#endif


type SqlReadSource m a = forall backend. SqlBackendCanRead backend =>
#if MIN_VERSION_conduit(1, 3, 0)
  ConduitT () a (ReaderT backend m) ()
#else
  Source (ReaderT backend m) a
#endif


type SqlReadConduit i m o = forall backend. SqlBackendCanRead backend =>
#if MIN_VERSION_conduit(1, 3, 0)
  ConduitT i o (ReaderT backend m) ()
#else
  Conduit i (ReaderT backend m) o
#endif


type SqlReadSink i m r = forall backend. SqlBackendCanRead backend =>
#if MIN_VERSION_conduit(1, 3, 0)
  ConduitT i Void (ReaderT backend m) r
#else
  Sink i (ReaderT backend m) r
#endif


#if MIN_VERSION_persistent(2, 8, 0)
type DBMonadBase m = (MonadUnliftIO m)
#else
type DBMonadBase m = (MonadBaseControl IO m)
#endif


class DBActionRunner a where
    -- | This should be a monad transformer
    type DBAction a :: (* -> *) -> * -> *
    runDBWith :: DBMonadBase m => a -> DBAction a m r -> m r


-- | Example: SomeDBActionRunner SqlPersistT
data SomeDBActionRunner t = forall a. (DBActionRunner a, DBAction a ~ t)
                            => SomeDBActionRunner a

runDBWith' :: DBMonadBase m
           => SomeDBActionRunner t
           -> t m r
           -> m r
runDBWith' (SomeDBActionRunner x) = runDBWith x


-- | A little "generalized" version of 'liftPersist'
runPersistEnvReaderT :: ( MonadReader env m
#if MIN_VERSION_persistent(2, 6, 0)
                        , HasPersistBackend env, BaseBackend env ~ r
#else
                        , HasPersistBackend env r
#endif
                        )
                     => ReaderT r m b -> m b
runPersistEnvReaderT f = do
  e <- ask
  runReaderT f (persistBackend e)


rnfEntity ::
    ( NFData a
#if MIN_VERSION_persistent(2, 0, 0)
    , NFData (Key a)
#else
    , NFData (KeyBackend (PersistEntityBackend a) a)
#endif
    ) =>
    Entity a -> ()
rnfEntity (Entity k v) = k `deepseq` v `deepseq` ()

type PersistQueryUniqueMonad backend n m =
#if MIN_VERSION_persistent(2, 0, 0)
    ( PersistUnique backend
    , PersistQuery backend
        -- PersistUnique/PersistQuery implies PersistStore
    , m ~ ReaderT backend n
    , MonadIO n
    )
#else
    ( PersistUnique m
    , PersistQuery m
        -- PersistUnique/PersistQuery implies PersistStore
    , backend ~ ()
    , n ~ []
    )
#endif

type IsPersistMonadOf backend n m val =
#if MIN_VERSION_persistent(2, 5, 0)
    ( PersistRecordBackend val backend
    , m ~ ReaderT backend n
    )
#elif MIN_VERSION_persistent(2, 0, 0)
    ( PersistEntityBackend val ~ backend
    , PersistEntity val
    , m ~ ReaderT backend n
    )
#else
    ( PersistEntityBackend val ~ PersistMonadBackend m
    , PersistEntity val
    , n ~ []
    , backend ~ ()
    )
#endif

type PersistUniqueMonad backend n m =
#if MIN_VERSION_persistent(2, 0, 0)
    ( PersistUnique backend
        -- PersistUnique implies PersistStore
    , m ~ ReaderT backend n
    , MonadIO n
    )
#else
    ( PersistUnique m
        -- PersistUnique implies PersistStore
    , backend ~ ()
    , n ~ []
    )
#endif

type PersistQueryMonad backend n m =
#if MIN_VERSION_persistent(2, 0, 0)
    ( PersistQuery backend
        -- PersistQuery implies PersistStore
    , m ~ ReaderT backend n
    , MonadIO n
    )
#else
    ( PersistQuery m
        -- PersistQuery implies PersistStore
    , backend ~ ()
    , n ~ []
    )
#endif

type PersistStoreMonad backend n m =
#if MIN_VERSION_persistent(2, 0, 0)
    ( PersistStore backend
    , m ~ ReaderT backend n
    , MonadIO n
    )
#else
    ( PersistStore m
    , backend ~ ()
    , n ~ []
    )
#endif

-- | try to insert a new record, replace existing one if unique constaint conflicts.
insertOrReplace' :: ( PersistQueryUniqueMonad backend n m
                    , IsPersistMonadOf backend n m val
#if MIN_VERSION_persistent(2, 10, 0)
                    , AtLeastOneUniqueKey val
#endif
                    )
                 => val
                 -> m (Either (Key val) (Key val))
                 -- ^ Left: the old key; Right: the new key
insertOrReplace' v = insertBy v
                        >>= either
                                (\(Entity k _) -> replace k v >> return (Left k))
                                (return . Right)


insertOrReplace :: ( PersistQueryUniqueMonad backend n m
                    , IsPersistMonadOf backend n m val
#if MIN_VERSION_persistent(2, 10, 0)
                    , AtLeastOneUniqueKey val
#endif
                    )
                 => val
                 -> m (Key val)
insertOrReplace = fmap (either id id) . insertOrReplace'


-- | select current records, replace them with the supplied new list.
-- Try hard to retain old records that are the same as new ones.
insertOrUpdateWithList ::
    ( Eq val
    , PersistQueryUniqueMonad backend n m
    , IsPersistMonadOf backend n m val
#if MIN_VERSION_persistent(2, 10, 0)
    , AtLeastOneUniqueKey val
#endif
    ) =>
    [Filter val]
    -> [val]                -- ^ new values
    -> m ([Key val], [Key val])
                            -- ^ new created keys and keys that are not touched
insertOrUpdateWithList fts new_ones = do
    old_entities <- selectList fts []
    let to_be_deleted = catMaybes $ flip map old_entities $ \(Entity k v) ->
                            if v `elem` new_ones
                                then Nothing
                                else Just k
    let to_be_inserted = catMaybes $ flip map new_ones $ \v ->
                            if v `elem` map entityVal old_entities
                                then Nothing
                                else Just v
    new_keys <- forM to_be_inserted $ \v -> do
                    insertOrReplace v
    return $ (new_keys, to_be_deleted L.\\ new_keys)


-- | like insertOrUpdateWithList, but also delete untouched keys.
replaceWithList ::
    ( Eq val
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val backend
#else
    , PersistEntityBackend val ~ backend
    , PersistEntity val
#endif

    , PersistUnique backend
    , PersistQuery backend
    , m ~ ReaderT backend n
    , MonadIO n

#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistUnique m
    , PersistQuery m
#endif
#if MIN_VERSION_persistent(2, 10, 0)
    , AtLeastOneUniqueKey val
#endif
    ) =>
    [Filter val]
    -> [val]                -- ^ new values
    -> m ([Key val], [Key val])
replaceWithList fts new_ones = do
    (new_keys, to_be_deleted) <- insertOrUpdateWithList fts new_ones
    mapM_ delete to_be_deleted
    return (new_keys, to_be_deleted)


-- | Automatically undo transaction when there is error throwError'ed
-- in wrapped function.
undoTransWhenE ::
#if MIN_VERSION_persistent(2, 0, 0)
    (MonadReader SqlBackend m, MonadIO m, MonadError e m) =>
    m a -> m a
#else
    (MonadSqlPersist m, MonadError e m) => m a -> m a
#endif
undoTransWhenE f = catchError f h
    where
        h err = liftPersist transactionUndo >> throwError err

#if MIN_VERSION_persistent(2, 0, 0)
undoTransWhenE2 :: (MonadIO m, MonadError e m) =>
                ReaderT SqlBackend m a
                -> ReaderT SqlBackend m a
undoTransWhenE2 f = catchError f h
    where
        h err = transactionUndo >> throwError err
#endif

-- | wrapped 'get', throwError when record does not exist.
getOrE ::
    ( PersistStore backend
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val backend
#else
    , PersistEntityBackend val ~ backend
    , PersistEntity val
#endif

    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistStore m
    , PersistEntityBackend val ~ PersistMonadBackend m
#endif
    ) =>
    e -> Key val -> ExceptT e m val
getOrE err k = lift (get k) >>= maybe (throwError err) return

#if MIN_VERSION_persistent(2, 0, 0)
getOrE2 :: ( PersistStore backend

#if MIN_VERSION_persistent(2, 5, 0)
            , PersistRecordBackend val backend
#else
            , PersistEntityBackend val ~ backend
            , PersistEntity val
#endif

            , MonadIO m
            , MonadError e m
            )
        => e
        -> Key val
        -> ReaderT backend m val
getOrE2 err k = get k >>= maybe (throwError err) return
#endif

-- | wrapped 'getBy', throwError when record does not exist.
getByOrE ::
    ( PersistUnique backend
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val backend
#else
    , PersistEntityBackend val ~ backend
    ,  PersistEntity val
#endif

    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistUnique m
#endif
    ) =>
    e -> Unique val -> ExceptT e m (Entity val)
getByOrE err k = lift (getBy k) >>= maybe (throwError err) return

#if MIN_VERSION_persistent(2, 0, 0)
getByOrE2 ::
    ( PersistUnique backend
#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val backend
#else
    , PersistEntityBackend val ~ backend
    , PersistEntity val
#endif

    , MonadIO m, MonadError e m
    ) =>
    e -> Unique val -> ReaderT backend m (Entity val)
getByOrE2 err k = getBy k >>= maybe (throwError err) return
#endif


-- | update or replace a record
insertOrUpdate ::
    ( PersistUnique backend
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val backend
#else
    , PersistEntityBackend val ~ backend
    , PersistEntity val
#endif

    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistUnique m, PersistQuery m
#endif

#if MIN_VERSION_persistent(2, 10, 0)
    , AtLeastOneUniqueKey val
#endif
    ) =>
    val                 -- ^ new value to insert
    -> [Update val]     -- ^ update commands to exec when old record
                        -- with the same unique key already exists.
    -> m (Either (Key val) (Key val))
                        -- ^ Left : the new created id
                        -- ^ Right: the id of old record that has been updated
insertOrUpdate v updates = do
    insertBy v >>= either (upd . entityKey) (return . Left)
    where
        upd k = update k updates >> return (Right k)


-- | Delete all records matching the given criterion
-- , except those whose keys in a specific list.
deleteWhereExcept ::
    ( PersistQuery backend
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val backend
#else
    , PersistEntityBackend val ~ backend
    , PersistEntity val
#endif

    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistStore m, PersistQuery m
#endif
    ) =>
    [Filter val]
    -> [Key val]
    -> m [Key val]
deleteWhereExcept filters keeps = do
    ks <- selectKeysList filters []
    let to_del = ks L.\\ keeps
    mapM_ delete to_del
    return to_del


type CachedInMap val m = StateT (Map (Key val) val) m

-- | when we don't should that might reselect the same record multiple times,
-- use this function to reduce DB access.
cget ::
    ( PersistStore backend
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val backend
#else
    , PersistEntityBackend val ~ backend
    , PersistEntity val
#endif

    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistStore m
#endif
    )
    => Key val
    -> CachedInMap val m (Maybe val)
cget k = do
    mv <- S.gets $ lookup k
    case mv of
        Just v -> return $ Just v
        Nothing -> do
            mv2 <- lift $ get k
            maybe (return ()) (S.modify . insertMap k) mv2
            return mv2

cselectList ::
    ( PersistQuery backend
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val backend
#else
    , PersistEntityBackend val ~ backend
    , PersistEntity val
#endif

    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistQuery m
#endif
    )
    => [Filter val]
    -> [SelectOpt val]
    -> CachedInMap val m [Entity val]
cselectList f o = do
    records <- lift $ selectList f o
    mapM_ cput records
    return records

cput ::
    (
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    PersistRecordBackend val backend
#else
    PersistEntityBackend val ~ backend
    , PersistEntity val
#endif

    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistStore m
#endif
    ) =>
    Entity val -> CachedInMap val m ()
cput (Entity k v) = S.modify $ insertMap k v


getFieldDBName :: PersistEntity a => EntityField a typ -> DBName
getFieldDBName = fieldDB . persistFieldDef

escFieldDBName :: PersistEntity a =>
#if MIN_VERSION_persistent(2, 0, 0)
    SqlBackend
#else
    Connection
#endif
    -> EntityField a typ -> Text
escFieldDBName conn = connEscapeName conn . getFieldDBName

escEntityDBName :: (PersistEntity a, Monad m) =>
#if MIN_VERSION_persistent(2, 0, 0)
    SqlBackend
#else
    Connection
#endif
    -> m a -> Text
escEntityDBName conn = connEscapeName conn . entityDB . entityDef


sinkEntityAsMap :: (Monad m, Ord (Key a))
#if MIN_VERSION_conduit(1, 3, 0)
                => ConduitT (Entity a) Void m (Map (Key a) a)
#else
                => Sink (Entity a) m (Map (Key a) a)
#endif
sinkEntityAsMap = go mempty
    where
        go s = do
            mx <- await
            case mx of
                Nothing             -> return s
                Just (Entity k v)   -> go $ insertMap k v s


-- | escape char that is considered as "special" in string with specified escape char
addEscape :: [Char] -> Char -> String -> String
addEscape   _           _   []      = []
addEscape   specials    esc (x:xs)  =
    let next = x : addEscape specials esc xs
    in if x == esc || x `elem` specials
        then esc : next
        else next

removeEscape :: [Char] -> Char -> String -> String
removeEscape    _        _   []          = []
removeEscape    _        _   [x]         = [x]
removeEscape    specials esc (x1:x2:xs)  =
    if x1 == esc
        then if x2 == esc || x2 `elem` specials
                then x2 : f xs
                else x2 : f xs  -- as tradition
        else x1 : f (x2:xs)
    where
        f = removeEscape specials esc

-- | Sepcial chars used in 'LIKE' pattern in SQL
-- XXX: This does not work for all DBMS
--      MS SQL Server has more special chars than others.
unsafeSpecialsForSqlLike :: [Char]
unsafeSpecialsForSqlLike = "%_"


unsafeEscapeForSqlLike :: String -> String
unsafeEscapeForSqlLike =
    -- XXX: 看 persistent 的代码，目前无法加上 ESCAPE 'x' 语句，
    -- 另一方面，不是所有DBMS都有缺省的转义字符（见Sqlite, Oracle, MS SQL Server）
    -- 还好，PostgreSQL MySQL 都有相同的缺省转义字符 '\'
    addEscape unsafeSpecialsForSqlLike '\\'

unsafeEscapeForSqlLikeT :: Text -> Text
unsafeEscapeForSqlLikeT = pack . unsafeEscapeForSqlLike . unpack

unsafeRemoveEscapeForSqlLike :: String -> String
unsafeRemoveEscapeForSqlLike = removeEscape unsafeSpecialsForSqlLike '\\'

unsafeRemoveEscapeForSqlLikeT :: Text -> Text
unsafeRemoveEscapeForSqlLikeT = pack . unsafeRemoveEscapeForSqlLike . unpack


contains :: EntityField v Text -> Text -> Filter v
contains = contains2 id

contains2 :: (PersistField a) =>
    (Text -> a)
    -> EntityField v a -> Text -> Filter v
contains2 conv field val = Filter field
#if MIN_VERSION_persistent(2, 10, 0)
                        (FilterValue v)
#else
                        (Left v)
#endif
                        (BackendSpecificFilter " LIKE ")
  where v = conv $ mconcat ["%", unsafeEscapeForSqlLikeT val, "%"]


-- | 包含并忽略大小写
containsI :: EntityField v Text -> Text -> Filter v
containsI = containsI2 id

-- | 包含并忽略大小写
containsI2 :: (PersistField a)
           => (Text -> a)
           -> EntityField v a
           -> Text
           -> Filter v
containsI2 conv field val = Filter field
#if MIN_VERSION_persistent(2, 10, 0)
                        (FilterValue v)
#else
                        (Left v)
#endif
                        (BackendSpecificFilter " ILIKE ")
  where v = conv $ mconcat ["%", unsafeEscapeForSqlLikeT val, "%"]


-- | Store data as native 'json' data type of DB engine
-- supported DB engines: PostgreSQL, MySQL
newtype PersistJson = PersistJson { unPersistJson :: Value }
  deriving (Show, Eq)

instance PersistFieldSql PersistJson where
  sqlType _ = SqlOther "JSON"

instance PersistField PersistJson where
  toPersistValue = PersistDbSpecific . toStrict . A.encode . unPersistJson

  fromPersistValue (PersistDbSpecific bs) = case A.eitherDecode' $ fromStrict bs of
                                              Left err -> Left $ fromString err
                                              Right x  -> Right $ PersistJson x

  fromPersistValue (PersistByteString bs) = case A.eitherDecode' $ fromStrict bs of
                                              Left err -> Left $ fromString err
                                              Right x  -> Right $ PersistJson x

  fromPersistValue x = Left $ "PersistJson must be converted from PersistDbSpecific, but got " <> tshow x

instance ToJSON PersistJson where toJSON = unPersistJson

instance FromJSON PersistJson where parseJSON = return . PersistJson


toPersistJson :: ToJSON a => a -> PersistJson
toPersistJson = PersistJson . toJSON


fromPersistJson :: FromJSON a => PersistJson -> Either String a
fromPersistJson (PersistJson v) = case A.fromJSON v of
                                    A.Error err -> Left err
                                    A.Success x -> Right x


-- | Store data as native 'jsonb' data type of postgresql
newtype PersistJsonb = PersistJsonb { unPersistJsonb :: Value }
  deriving (Show, Eq)

instance PersistFieldSql PersistJsonb where
  sqlType _ = SqlOther "JSONB"

instance PersistField PersistJsonb where
  toPersistValue = PersistDbSpecific . toStrict . A.encode . unPersistJsonb

  fromPersistValue (PersistDbSpecific bs) = case A.eitherDecode' $ fromStrict bs of
                                              Left err -> Left $ fromString err
                                              Right x  -> Right $ PersistJsonb x

  fromPersistValue (PersistByteString bs) = case A.eitherDecode' $ fromStrict bs of
                                              Left err -> Left $ fromString err
                                              Right x  -> Right $ PersistJsonb x

  fromPersistValue x = Left $ "PersistJsonb must be converted from PersistDbSpecific, but got " <> tshow x

instance ToJSON PersistJsonb where toJSON = unPersistJsonb

instance FromJSON PersistJsonb where parseJSON = return . PersistJsonb


toPersistJsonb :: ToJSON a => a -> PersistJsonb
toPersistJsonb = PersistJsonb . toJSON


fromPersistJsonb :: FromJSON a => PersistJsonb -> Either String a
fromPersistJsonb (PersistJsonb v) = case A.fromJSON v of
                                    A.Error err -> Left err
                                    A.Success x -> Right x

newtype PersistDiffTime = PersistDiffTime { unPersistDiffTime :: NominalDiffTime }
  deriving (Show, Eq, Num)

instance PersistFieldSql PersistDiffTime where
  sqlType _ = SqlOther "INTERVAL"

instance PersistField PersistDiffTime where
  toPersistValue = PersistDbSpecific . toStrict . toLazyByteString . nominalDiffTimeToBuilder . unPersistDiffTime
  fromPersistValue (PersistDbSpecific bs) = Left $ "TBD: " <> decodeUtf8 bs -- TODO: decode interval from different styles output
  fromPersistValue x = Left $ "PersistDiffTime must be converted from PersistDbSpecific, but got " <> tshow x

findByEntityKey :: (Element seq ~ Entity record, SemiSequence seq, Eq (Key record))
                => Key record
                -> seq
                -> Maybe (Entity record)
findByEntityKey k = find ((== k) . entityKey)


entitiesToMap :: (ContainerKey c ~ Key record, MapValue c ~ record, IsMap c)
              => [Entity record]
              -> c
entitiesToMap = mapFromList . map (entityKey &&& entityVal)


-- | 例如可以把 'get' 的返回值变成 Maybe (Entity a)
attachEntityKey :: (PersistEntity record, Functor m, Functor f)
                => (Key record -> m (f record))
                -> Key record
                -> m (f (Entity record))
attachEntityKey = attachEntityKey' Entity


attachEntityKey' :: (PersistEntity record, Functor m, Functor f)
                 => (Key record -> record -> a)
                 -> (Key record -> m (f record))
                 -> Key record
                 -> m (f a)
attachEntityKey' h g rec_id = fmap (fmap (h rec_id)) $ g rec_id


-- | 含有 deleted 字段的 PersistEntity
class PersistEntity a => HasEntityFieldDeleted a where
  entityFieldDeleted :: EntityField a Bool

deriveHasEntityFieldDeleted :: Name -> DecsQ
deriveHasEntityFieldDeleted n = do
  let deleted_field = mkName $ nameBase n <> "Deleted"
  return
    [ InstanceD Nothing [] (ConT ''HasEntityFieldDeleted `AppT` ConT n)
      [ FunD 'entityFieldDeleted
        [ Clause [] (NormalB $ ConE deleted_field) []
        ]
      ]
    ]

selectListWithDeleted :: ( HasEntityFieldDeleted a, PersistQueryMonad backend n m
                         , IsPersistMonadOf backend n m a
                         )
                      => Bool
                      -> [ Filter a ]
                      -> [ SelectOpt a ]
                      -> m [Entity a]
selectListWithDeleted is_deleted filters opts =
  selectList ((entityFieldDeleted ==. is_deleted) : filters) opts

selectKeysListWithDeleted :: ( HasEntityFieldDeleted a, PersistQueryMonad backend n m
                             , IsPersistMonadOf backend n m a
                             )
                          => Bool
                          -> [ Filter a ]
                          -> [ SelectOpt a ]
                          -> m [Key a]
selectKeysListWithDeleted is_deleted filters opts =
  selectKeysList ((entityFieldDeleted ==. is_deleted) : filters) opts


-- | insert record, retry when new record when insertUniqueEntity failed.
-- CAUTION: 若 a 包含多个 Unique a, 可能有死循环．请用 checkUniqueInsertRetry
insertUniqueEntityRetry :: ( PersistUniqueMonad backend n m
                           , IsPersistMonadOf backend n m a
                           )
                        => (a -> m (Maybe a))
                        -> a
                        -> m (Maybe (Entity a))
insertUniqueEntityRetry mk_new rec0 = loop rec0
  where
    loop rec = do
      m_ent <- insertUniqueEntity rec
      case m_ent of
        Just x -> return $ Just x
        Nothing -> mk_new rec >>= fmap join . mapM loop


-- | 效果类似于 insertUniqueEntityRetry，不过能处理有多个unique存在的情况
checkUniqueInsertRetry :: ( PersistUniqueMonad backend n m
                          , IsPersistMonadOf backend n m a
                          , Eq (Unique a)
                          )
                       => (a -> Unique a)
                       -> (a -> m (Maybe a))
                       -> a
                       -> m (Either (Unique a) (Maybe (Entity a)))
checkUniqueInsertRetry get_unique mk_new rec0 = loop rec0
  where
    loop rec = do
      m_unique <- checkUnique rec
      case m_unique of
        Nothing -> fmap (Right . Just) $ insertEntity rec
        Just u -> if u == get_unique rec
                     then mk_new rec >>= fmap (fromMaybe $ Right Nothing) . mapM loop
                     else return $ Left u


-- | 常见的自动改文件名策略: 在旧文件名后加上 (n) 的字串，n根据前一个文件名中的数字增加一
autoNameNumAscending :: FilePath -> FilePath
autoNameNumAscending p = replaceBaseName p new_base_name
  where base_name = takeBaseName p

        (file_real_name, file_seq_num) = fromMaybe (base_name, 0) split_num_part
        new_base_name = file_real_name <> "(" <> show (file_seq_num + 1) <> ")"

        split_num_part :: Maybe (FilePath, Integer)
        split_num_part = do
          s1 <- stripSuffix ")" base_name
          let s2 = dropWhileEnd isDigit s1
          real_name <- stripSuffix "(" s2
          guard $ not $ null real_name
          num_part <- stripPrefix s2 s1
          num <- readMay num_part
          guard $ num > 0 && num < 9999
          pure (real_name, num)


-- vim: set foldmethod=marker:
