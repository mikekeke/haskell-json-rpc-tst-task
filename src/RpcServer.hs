{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RpcServer
  ( startRpcServer
  ) where

import           Database.Persist.TH

import           AppConfig
import           Utils

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Text
import           Domain
import           Logging
import           Network.JsonRpc.Server
import           System.IO                  (BufferMode (LineBuffering),
                                             hSetBuffering, stdout)

import           Conduit                    (ResourceT, runResourceT)
import           Control.Monad.Catch
import           Control.Monad.Except       (ExceptT, throwError)
import           Data.Aeson                 (toJSON)
import           Data.Maybe                 (fromMaybe)
import           Data.UUID.V4               (nextRandom)
import           Database.Persist           (entityVal, insert, insert_,
                                             selectList)
import           Database.Persist.Sql       (SqlBackend, runSqlPersistM)
import           Database.Persist.Sqlite    (fromSqlKey, runMigration,
                                             runSqlConn, withSqliteConn,
                                             wrapConnection)
import           Database.Sqlite            (open)
import           System.Exit                (exitSuccess)

addUserRpcErrCode = -32001

getAllRpcErrCode = -32002

validateUserRpcErrCode = -32003

exitCommand = "quit"

helpText =
  "\nAdd user example:\n\
   \  {\"jsonrpc\": \"2.0\", \"method\": \"addUser\", \"params\": {\"name\": \"Bob\", \"age\": 42}, \"id\": 3}\n\
   \List users example:\n\
   \  {\"jsonrpc\": \"2.0\", \"method\": \"getUsers\", \"id\": 3}\n\
   \(user name has unique constraint to test SQL error)"

logAndThrowRpc errCode msg ex = do
  txtUid <- toText <$> liftIO nextRandom
  logErrorN $ mconcat [txtUid, " ", toText ex]
  throwError . rpcError errCode $ mconcat [msg, ". Error UUID: ", txtUid]

type Server = ReaderT SqlBackend (LoggingT (ResourceT IO))

add = toMethod "addUser" f (Required "name" :+: Required "age" :+: ())
  where
    f uName uAge = do
      usr <- parseUser uName uAge `catchAll` logAndThrowRpc validateUserRpcErrCode "Invalid user data"
      userId <- lift (insert usr) `catchAll` logAndThrowRpc addUserRpcErrCode "Error adding user"
      logInfoN $ mconcat ["New user added. ID: ", toText $ fromSqlKey userId]
      return userId

get = toMethod "getUsers" f ()
  where
    f = lift action `catchAll` logAndThrowRpc getAllRpcErrCode "Error retreiving users"
    action = fmap entityVal <$> selectList [] [] :: Server [User]

methods :: [Method Server]
methods = [add, get]

initDb :: IO SqlBackend
initDb = do
  conn <- open (dbConn appConf)
  (backend :: SqlBackend) <- wrapConnection conn (\_ _ _ _ -> return ())
  runSqlConn (runMigration migrateAll) backend
  return backend

runApp backend =
  runResourceT . runStderrLoggingT . applyAllLogFilters $ do
    liftIO $ hSetBuffering stdout LineBuffering
    contents <- liftIO B.getContents
    logInfoN "Initializing DB"
    liftIO $ putStrLn helpText
    forM_ (B.lines contents) $ \request -> do
      response <- runSqlConn (call methods request) backend
      liftIO $ B.putStrLn $ fromMaybe "" response

startRpcServer = initDb >>= runApp
