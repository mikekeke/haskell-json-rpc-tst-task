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
import           Database.Persist           (entityVal, insert, insert_,
                                             selectList)
import           Database.Persist.Sql       (SqlBackend)
import           Database.Persist.Sqlite    (runMigration, runSqlConn,
                                             withSqliteConn)
import           GHC.IO                     (catchAny)

type SqlConn = Text -- mk as text like from those blogpost

type SqlLayer = ReaderT SqlBackend (LoggingT (ResourceT IO))

instance UserStorage (ExceptT RpcError SqlLayer) where
  saveUser u = do
    r <- lift $ try (insert u)
    either onError onSuccess r
    where
      onError (e :: SomeException) =
        logErrorN (toText e) >> throwError (rpcError (-32001) "Failed to add user. Error UUID = XXX")
      onSuccess uId = logInfoN (mconcat ["New user added with ID ", toText . getId $ uId])

--  getAll = fmap entityVal <$> lift (selectList [] [])
--  getAll = lift act where
--    act = fmap entityVal <$> selectList [] []
instance CheckUser (ExceptT RpcError SqlLayer) where
  validateAge a = do
    unless (isLegalAge a) $ throwError (rpcError (-32000) (toText $ IllegalUserAge a))
    return $ ValidAge a

methods :: [Method SqlLayer]
--methods = [addUserMet, listUsers]
methods = [addUserMet]

addUserMet = toMethod "addUser" f (Required "name" :+: Required "age" :+: ())
  where
    f :: String -> Int -> RpcResult SqlLayer Text
    f uName uAge = do
      createUser (pack uName) uAge
      return "User added"

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
        cmd <- liftIO getLine
        logInfoN $ mconcat ["RPC call: ", pack cmd]
        r <- call methods (B.pack cmd)
        liftIO (printRpcResult r)
