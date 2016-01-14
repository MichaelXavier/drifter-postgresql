{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Exception
import           Data.Monoid
import           Database.PostgreSQL.Simple
import           Drifter                    (Change (..), ChangeName (..),
                                             changeSequence)
import           Drifter.PostgreSQL         (Method (..), PGMigration (..),
                                             runMigrations)
-------------------------------------------------------------------------------


main :: IO ()
main = bracket (connect defaultConnectInfo) close $ \conn -> do
  either error return =<< runMigrations conn changes


-------------------------------------------------------------------------------
changes :: [Change PGMigration]
changes = changeSequence [ createTable
                         , addIndexes
                         , fireMissiles
                         ]


-------------------------------------------------------------------------------
createTable :: Change PGMigration
createTable = Change { changeName = ChangeName "create table"
                     , changeDescription = Just "create some tables"
                     -- we'll have changeSequence sort out our dependencies
                     , changeDependencies = []
                     -- Query is a monoid, we can 'mappend' or even 'mconcat' them
                     , changeMethod = MigrationQuery (q1 <> q2)
                     }
  where q1 = "CREATE TABLE foo ( name pgtext );"
        q2 = "CREATE TABLE bar ( name pgtext );"


-------------------------------------------------------------------------------
addIndexes :: Change PGMigration
addIndexes = Change { changeName = ChangeName "add indexes"
                    , changeDescription = Just "add some indexes"
                    , changeDependencies = []
                    , changeMethod = MigrationQuery q
                    }
  where q = "CREATE UNIQUE INDEX foo_name ON foo ( name );"


-------------------------------------------------------------------------------
fireMissiles :: Change PGMigration
fireMissiles = Change { changeName = ChangeName "fire missiles"
                      , changeDescription = Just "a migration that isn't a query"
                      , changeDependencies = []
                      , changeMethod = MigrationCode doEvil
                      }
  where doEvil :: Connection -> IO (Either String ())
        doEvil _ = error "boom!"
