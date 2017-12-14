{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative              as A
import           Control.Monad
import           Data.IORef
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Drifter
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Drifter.PostgreSQL
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain $ testGroup "drifter-postgresql"
  [
    withResource setup teardown $ \getConn -> testCase "migrations" $ do
       c <- getConn
       c2Calls <- newIORef 0
       let migrate' = runMigrations c
       res <- migrate' [c1]
       res @?= Right ()
       rows <- query_ c "SELECT x FROM c1;"
       rows @?= ([Only "c1"] :: [Only Text])

       res' <- migrate' [c1, c2 c2Calls]
       res' @?= Right ()
       calls <- readIORef c2Calls
       calls @?= 1

       res'' <- migrate' [c1, c2 c2Calls]
       res'' @?= Right ()

       calls' <- readIORef c2Calls
       calls' @?= 1
  ]

-------------------------------------------------------------------------------
c1 :: Change PGMigration
c1 = Change (ChangeName "c1") (Just "c1 migration") [] meth
  where
    meth = MigrationQuery q
    q = [sql|
          CREATE TABLE c1 (
            id serial NOT NULL,
            x text NOT NULL,

            PRIMARY KEY (id)
          );

          INSERT INTO c1 (x) VALUES ('c1');
        |]


-------------------------------------------------------------------------------
c2 :: IORef Int -> Change PGMigration
c2 ref = Change (ChangeName "c2") (Just "c2 migration") [changeName c1] meth
  where
    meth = MigrationCode (\_ -> Right A.<$> modifyIORef' ref succ)


-------------------------------------------------------------------------------
setup :: IO Connection
setup = do
    c <- connect defaultConnectInfo
    void $ execute_ c "DROP DATABASE IF EXISTS drifter_test;"
    void $ execute_ c "CREATE DATABASE drifter_test;"
    close c
    c' <- connect defaultConnectInfo { connectDatabase = "drifter_test" }
    return c'


-------------------------------------------------------------------------------
teardown :: Connection -> IO ()
teardown = close
