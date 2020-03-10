{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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
import           Control.Monad.Catch
import           Control.Monad.Except       (ExceptT, throwError)
import           Data.Aeson                 (ToJSON, object, toJSON, (.=))
import           Data.Bool                  (bool)
import           Data.Maybe                 (fromMaybe)
import           Database.Persist           (entityVal, insert_, selectList)
import           Database.Persist.Sql       (SqlBackend)
import           Database.Persist.Sqlite    (runMigration, runSqlConn,
                                             withSqliteConn)
import           GHC.IO                     (catchAny)

type SqlConn = Text -- mk as text like from those blogpost

type SqlLayer = ReaderT SqlBackend (LoggingT (ResourceT IO))

instance UserStorage (ExceptT RpcError SqlLayer) where --todo maybe just SqlLayer will be enough?
  saveUser u = lift action
    where
      action = (insert_ u >> return (Right u)) `catch` (\(e :: SomeException) -> return (Left . SqlFail . toText $ e))

--  getAll = fmap entityVal <$> lift (selectList [] [])
--  getAll = lift act where
--    act = fmap entityVal <$> selectList [] []
instance CheckUser (ExceptT RpcError SqlLayer) where
  validateAge a = do
    unless (isLegalAge a) $ throwError (rpcError (-32000) (toText $ IllegalUserAge a))
    return (Right $ ValidAge a)

methods :: [Method SqlLayer]
--methods = [addUserMet, listUsers]
methods = [addUserMet]

addUserMet = toMethod "addUser" f (Required "name" :+: Required "age" :+: ())
  where
    reportError e = logErrorN (toText e) >> throwError (rpcError (-32001) "Failed to add user. Error UUID = XXX")
    success message u = logInfoN (mconcat [message, " : ", toText u]) >> return message
    f :: String -> Int -> RpcResult SqlLayer Text
    f uName uAge = do
      u <- createUser (pack uName) uAge
      either reportError (success "User added") u

--listUsers = toMethod "list" _  () where
--  act = (getAll :: RpcResult SqlLayer [User])
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
      forever $ -- todo exit command
       do
        cmd <- B.pack <$> liftIO getLine
        r <- call methods cmd
        liftIO (printRpcResult r)
