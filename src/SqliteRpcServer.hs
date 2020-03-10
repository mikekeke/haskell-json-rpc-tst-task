{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SqliteRpcServer where

import           BusinessLogic
import           Utils

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Text
import           Logging
import           Models
import           Network.JsonRpc.Server
import           System.IO                  (BufferMode (LineBuffering),
                                             hSetBuffering, stdout)

import           Conduit                    (ResourceT, runResourceT)
import           Control.Monad.Except       (ExceptT, throwError)
import           Data.Aeson                 (ToJSON, object, toJSON, (.=))
import           Data.Bool                  (bool)
import           Data.Maybe                 (fromMaybe)
import           Database.Persist           (entityVal, insert_, selectList)
import           Database.Persist.Sql       (SqlBackend)
import           Database.Persist.Sqlite    (runMigration, runSqlConn,
                                             withSqliteConn)

data ServerError =
  IllegalUserAge Int
  deriving (Show)

type SqlConn = Text -- mk as text like from those blogpost

type SqlLayer = ReaderT SqlBackend (LoggingT (ResourceT IO))

instance UserStorage (ExceptT RpcError SqlLayer) where
  saveUser = lift . insert_ -- todo handle sql errors
  getAll = fmap entityVal <$> lift (selectList [] [])

instance CheckUser (ExceptT RpcError SqlLayer) where
  checkAge a = do
    unless (isLegalAge a) $ throwError (rpcError (-32000) (toText $ IllegalUserAge a))
    return (ValidAge a)

methods :: [Method SqlLayer]
methods = [addUserMet, listUsers]

addUserMet = toMethod "addUser" f (Required "name" :+: Required "age" :+: ())
  where
    f :: String -> Int -> RpcResult SqlLayer Text
    f uName uAge = do
      u <- createUser (pack uName) uAge
      let message = "User added"
      logInfoN (mconcat [message, " : ", toText u])
      return message

listUsers = toMethod "list" (getAll :: RpcResult SqlLayer [User]) ()

runServer :: SqlConn -> SqlLayer a -> IO a
runServer connStr = runResourceT . runStderrLoggingT . applyAllLogFilters . withSqliteConn connStr . runSqlConn

printRpcResult = B.putStrLn . fromMaybe ""

runSqliteServer :: IO ()
runSqliteServer = do
  hSetBuffering stdout LineBuffering
  void $
    runServer ":memory:" $ do
      logInfoN "Starting server"
      runMigration migrateAll
      liftIO $ print "Enter commands:"
      forever $ do -- todo exit command
        cmd <- B.pack <$> liftIO getLine
        r <- call methods cmd
        liftIO (printRpcResult r)
