{-# LANGUAGE ScopedTypeVariables #-}
module Database.Persist.TX.Utils.Postgres where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Logger
import           Data.Proxy
import           Database.Persist
import           Database.Persist.Sql
#if MIN_VERSION_persistent(2, 12, 0)
import           Database.Persist.SqlBackend.Internal (connEscapeRawName)
#endif
import           Control.Monad.Trans.Maybe
import qualified Database.PostgreSQL.Simple as PGS
-- }}}1


data PgColumnInfo = PgColumnInfo
  { pgColInfoName       ::
#if MIN_VERSION_persistent(2, 12, 0)
                           FieldNameDB
#else
                           DBName
#endif
  , pgColInfoOrdinalNum :: Int
  }

pgSqlGetTableColumnInfo :: forall record m.
                          ( PersistEntity record, MonadIO m
                          , PersistEntityBackend record ~ SqlBackend
                          )
                        => Proxy record
                        -> SqlPersistT m [PgColumnInfo]
-- {{{1
pgSqlGetTableColumnInfo _ = do
  rows <- rawSql sql [ toPersistValue table_name_s ]

  return $
    flip map rows $ \ (Single name, Single num) ->
      PgColumnInfo
#if MIN_VERSION_persistent(2, 12, 0)
        (FieldNameDB name)
#else
        (DBName name)
#endif
        num

  where
    sql = "SELECT attname, attnum FROM pg_attribute WHERE attrelid = ? :: regclass"

    dummy_rec = error "entity record forced" :: record

    table_name_s =
#if MIN_VERSION_persistent(2, 12, 0)
        unEntityNameDB
#else
        unDBName
#endif
           $ tableDBName dummy_rec

-- }}}1


data PgIndexInfo = PgIndexInfo
  { pgIndexInfoName      :: Text
  , pgIndexInfoIsUnique  :: Bool
  , pgIndexInfoIsPrimary :: Bool
  }


data SomeEntityField record = forall typ. SomeEntityField (EntityField record typ)

pgSqlGetIndexInfoByFields :: forall record m.
                            ( PersistEntity record
                            , PersistEntityBackend record ~ SqlBackend
                            , MonadIO m, MonadLogger m
                            )
                          => [SomeEntityField record]
                          -> SqlPersistT m (Maybe PgIndexInfo)
-- {{{1
pgSqlGetIndexInfoByFields fields = runMaybeT $ do
  col_infos <- lift $ pgSqlGetTableColumnInfo (Proxy :: Proxy record)
  ord_num_list <- forM fields $ \ (SomeEntityField ef) -> do
                    let field_name = fieldDBName ef
                    case find ((field_name ==) . pgColInfoName) col_infos of
                      Just info -> return $ pgColInfoOrdinalNum info

                      Nothing -> do
                        $logError $ "field '" <> un_field_name field_name <> "' does not exist."
                        mzero

  let indkey_str = intercalate " " (map tshow ord_num_list)

  rows <- lift $ rawSql
                  "SELECT indexrelid,indisunique,indisprimary FROM pg_index WHERE indrelid = ? :: regclass AND indislive=? AND indkey=?"
                  [ toPersistValue $ un_table_name $ tableDBName dummy_rec
                  , toPersistValue True
                  , toPersistValue indkey_str
                  ]

  when (null rows) $ do
    $logDebug $ "Table index does not exist: " <> indkey_str <> " on table " <> un_table_name (tableDBName dummy_rec)

#if MIN_VERSION_persistent(2, 11, 0)
  (Single (PersistLiteralEscaped idx_oid), Single is_unique, Single is_primary)
#else
  (Single (PersistDbSpecific idx_oid), Single is_unique, Single is_primary)
#endif
      <- MaybeT $ return $ listToMaybe rows

  rows2 <- lift $ rawSql
                    "SELECT relname FROM pg_class WHERE oid = ?"
                    [
#if MIN_VERSION_persistent(2, 11, 0)
                      toPersistValue (PersistLiteralEscaped idx_oid)
#else
                      toPersistValue (PersistDbSpecific idx_oid)
#endif
                    ]

  when (null rows) $ do
    $logDebug $ "Oid of table index does not exist: " <> indkey_str <> " on table " <> un_table_name (tableDBName dummy_rec)
                <> ", oid=" <> decodeUtf8 idx_oid

  Single idx_name <- MaybeT $ return $ listToMaybe rows2

  return $ PgIndexInfo idx_name is_unique is_primary

  where
    dummy_rec = error "entity record forced" :: record

    un_field_name =
#if MIN_VERSION_persistent(2, 12, 0)
                    unFieldNameDB
#else
                    unDBName
#endif

    un_table_name =
#if MIN_VERSION_persistent(2, 12, 0)
                    unEntityNameDB
#else
                    unDBName
#endif
-- }}}1


data CreateIndexOpt = CreateIndexUnique
                    | CreateIndexConcurrently
                    deriving (Show, Eq, Ord)

pgSqlCreateIndexInfoByFields :: forall record m opts_t.
                               ( PersistEntity record
                               , PersistEntityBackend record ~ SqlBackend
                               , MonadIO m, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 0, 0)
                               , MonoFoldable opts_t
#else
                               , MonoFoldableEq opts_t
#endif
                               , Element opts_t ~ CreateIndexOpt
                               )
                             => Maybe Text
                             -> opts_t
                             -> [SomeEntityField record]
                             -> SqlPersistT m ()
-- {{{1
pgSqlCreateIndexInfoByFields m_index_name opts fields = do
  conn <- ask
  table_name <- getTableName dummy_rec
  field_names <- forM fields $ \ (SomeEntityField ef) -> getFieldName ef
  let field_names_sql = intercalate "," field_names

  let sql = intercalate " " $ filter (not . null) $
              [ "CREATE"
              , if CreateIndexUnique `oelem` opts
                   then "UNIQUE"
                   else ""
              , "INDEX"
              , if CreateIndexConcurrently `oelem` opts
                   then "CONCURRENTLY"
                   else ""
              , fromMaybe "" $ conn_escape_name conn <$> m_index_name
              , "ON"
              , table_name
              , "("
              , field_names_sql
              , ")"
              ]

  rawExecute sql []
  where
    dummy_rec = error "entity record forced" :: record

    conn_escape_name =
#if MIN_VERSION_persistent(2, 12, 0)
                    connEscapeRawName
#else
                    \ x -> connEscapeName x . DBName
#endif
-- }}}1


pgSqlCreateIndexInfoByFieldsIfNotExist :: forall record m opts_t.
                                         ( PersistEntity record
                                         , PersistEntityBackend record ~ SqlBackend
                                         , MonadIO m, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 0, 0)
                                         , MonoFoldable opts_t
#else
                                         , MonoFoldableEq opts_t
#endif
                                         , Element opts_t ~ CreateIndexOpt
                                         )
                                       => Maybe Text
                                       -> opts_t
                                       -> [SomeEntityField record]
                                       -> SqlPersistT m ()
-- {{{1
pgSqlCreateIndexInfoByFieldsIfNotExist m_index_name opts fields = do
  m_info <- pgSqlGetIndexInfoByFields fields
  case m_info of
    Nothing -> pgSqlCreateIndexInfoByFields m_index_name opts fields
    Just info -> do
      case m_index_name of
        Nothing -> return ()
        Just index_name -> do
          let old_name = pgIndexInfoName info

          unless (old_name == index_name) $ do
            $logWarn $ "An index with the same fields exists but with a different name (creation cancelled): " <> old_name
-- }}}1


pgSqlCreateIndexInfoByFieldsIfNotExist1 :: forall record m opts_t typ1.
                                         ( PersistEntity record
                                         , PersistEntityBackend record ~ SqlBackend
                                         , MonadIO m, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 0, 0)
                                         , MonoFoldable opts_t
#else
                                         , MonoFoldableEq opts_t
#endif
                                         , Element opts_t ~ CreateIndexOpt
                                         )
                                       => Maybe Text
                                       -> opts_t
                                       -> EntityField record typ1
                                       -> SqlPersistT m ()
-- {{{1
pgSqlCreateIndexInfoByFieldsIfNotExist1 m_index_name opts field1 = do
  pgSqlCreateIndexInfoByFieldsIfNotExist m_index_name opts
    ([SomeEntityField field1] :: [SomeEntityField record])
-- }}}1


pgSqlCreateIndexInfoByFieldsIfNotExist2 :: forall record m opts_t typ1 typ2.
                                         ( PersistEntity record
                                         , PersistEntityBackend record ~ SqlBackend
                                         , MonadIO m, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 0, 0)
                                         , MonoFoldable opts_t
#else
                                         , MonoFoldableEq opts_t
#endif
                                         , Element opts_t ~ CreateIndexOpt
                                         )
                                       => Maybe Text
                                       -> opts_t
                                       -> EntityField record typ1
                                       -> EntityField record typ2
                                       -> SqlPersistT m ()
-- {{{1
pgSqlCreateIndexInfoByFieldsIfNotExist2 m_index_name opts field1 field2 = do
  pgSqlCreateIndexInfoByFieldsIfNotExist m_index_name opts
    ([ SomeEntityField field1, SomeEntityField field2] :: [SomeEntityField record])
-- }}}1


pgSqlConnTxSetIsolationSerializable :: MonadIO m => PGS.Connection -> m ()
pgSqlConnTxSetIsolationSerializable conn = liftIO $
  void $ PGS.execute_ conn "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL SERIALIZABLE"
