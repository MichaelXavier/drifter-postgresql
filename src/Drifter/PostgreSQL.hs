{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Drifter.PostgreSQL
    ( PGMigration
    , Method(..)
    , DBConnection(..)
    , ChangeHistory(..)
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Drifter.Types
-------------------------------------------------------------------------------


data PGMigration


data instance Method PGMigration = MigrationQuery Query
                                 | MigrationCode (Connection -> IO (Either String ()))


data instance DBConnection PGMigration = DBConnection Connection


instance Drifter PGMigration where
  migrate (DBConnection c) cs = do
    void $ execute_ c bootstrapQ
    withTransaction c $ do
      hist <- query_ c changeHistoryQ
      start <- findNext hist cs
      runEitherT $ mapM_ (migrateChange c) start


-------------------------------------------------------------------------------
-- Change History Tracking
-------------------------------------------------------------------------------
newtype ChangeId = ChangeId Int deriving (Eq, Ord, Show, FromField)


data ChangeHistory = ChangeHistory {
      histId          :: ChangeId
    , histName        :: Name
    , histDescription :: Maybe Description
    , histTime        :: UTCTime
    } deriving (Show)


instance Eq ChangeHistory where
    a == b = (histName a) == (histName b)


instance Ord ChangeHistory where
    compare a b = compare (histId a) (histId b)


instance FromRow ChangeHistory where
    fromRow = ChangeHistory <$> field <*> field <*> field <*> field


-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------
bootstrapQ :: Query
bootstrapQ = [sql|
CREATE TABLE IF NOT EXISTS schema_migrations (
    id              serial      NOT NULL,
    name            text        NOT NULL,
    description     text,
    time            timestamptz NOT NULL DEFAULT now(),

    PRIMARY KEY (id),
    UNIQUE (name)
);
|]


-------------------------------------------------------------------------------
changeHistoryQ :: Query
changeHistoryQ =
  "SELECT id, name, description, time FROM schema_migrations ORDER BY id;"


-------------------------------------------------------------------------------
insertLogQ :: Query
insertLogQ =
  "INSERT INTO schema_migrations (name, description, time) VALUES (?, ?, ?);"


-------------------------------------------------------------------------------
findNext :: [ChangeHistory] -> [Change PGMigration] -> IO [Change PGMigration]
findNext [] cs = return cs
findNext (h:hs) (c:cs)
  | (histName h) == (changeName c) = do
    putStrLn $ "Skipping: " ++ show (changeName c)
    findNext hs cs
  | otherwise = return (c:cs)
findNext _ _ = do
  putStrLn "Change Set Exhausted"
  return []


-------------------------------------------------------------------------------
migrateChange :: Connection -> Change PGMigration -> EitherT String IO ()
migrateChange c ch@Change{..} = do
  runMethod c changeMethod

  logChange c ch
  lift $ putStrLn $ "Committed: " ++ show changeName


-------------------------------------------------------------------------------
runMethod :: Connection -> Method PGMigration -> EitherT String IO ()
runMethod c (MigrationQuery q) =
  void $ EitherT $ (Right <$> execute_ c q) `catches` errorHandlers
runMethod c (MigrationCode f) =
  EitherT $ f c `catches` errorHandlers


  -------------------------------------------------------------------------------
logChange :: Connection -> Change PGMigration -> EitherT String IO ()
logChange c Change{..} = do
    now <- lift getCurrentTime
    void $ EitherT $ (Right <$> go now) `catches` errorHandlers
  where
    go now = execute c insertLogQ (changeName, changeDescription, now)


-------------------------------------------------------------------------------
errorHandlers :: [Handler (Either String b)]
errorHandlers = [ Handler (\(ex::SqlError) -> return $ Left $ show ex)
                , Handler (\(ex::FormatError) -> return $ Left $ show ex)
                , Handler (\(ex::ResultError) -> return $ Left $ show ex)
                , Handler (\(ex::QueryError) -> return $ Left $ show ex)
                ]
