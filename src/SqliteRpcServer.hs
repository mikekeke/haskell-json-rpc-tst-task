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
import           Data.Maybe                 (fromMaybe)
import           Data.UUID.V4               (nextRandom)
import           Database.Persist           (entityVal, insert, insert_,
                                             selectList)
import           Database.Persist.Sql       (SqlBackend)
import           Database.Persist.Sqlite    (runMigration, runSqlConn,
                                             withSqliteConn)
import           System.Exit                (exitSuccess)

type SqlConn = Text -- mk as text like in those blogpost

type SqlLayer = ReaderT SqlBackend (LoggingT (ResourceT IO))

userSaveRpcErrCode = -32001

getAllRpcErrCode = -32002

validateUserRpcErrCode = -32003

exitCommand = "quit"

isExitCommand = (exitCommand ==)

helpText =
  "\nAdd user e.g.:\n\
            \  {\"jsonrpc\": \"2.0\", \"method\": \"addUser\", \"params\": {\"name\": \"Bob\", \"age\": 42}, \"id\": 3}\n\
            \List users e.g.:\n\
            \  {\"jsonrpc\": \"2.0\", \"method\": \"getUsers\", \"id\": 3}\n\
            \(user name has unique constraint to test SQL error)\n\
            \Type \"" ++
  exitCommand ++ "\" to exit"

logAndThrow ex errCode msg = do
  txtUid <- toText <$> liftIO nextRandom
  logErrorN $ mconcat [txtUid, " ", toText ex]
  throwError . rpcError errCode $ mconcat [msg, ". Error UUID: ", txtUid]

instance UserStorage (ExceptT RpcError SqlLayer) where
  saveUser u = lift (try $ insert u) >>= either onError onSuccess
    where
      onError (e :: SomeException) = logAndThrow e userSaveRpcErrCode "Failed to add user"
      onSuccess uId = logInfoN (mconcat ["New user added with ID ", toText . getId $ uId])
  getAll = lift (try $ selectList [] []) >>= either onError onSuccess
    where
      onError (e :: SomeException) = logAndThrow e getAllRpcErrCode "Failed get users"
      onSuccess = return . fmap entityVal

instance CheckUser (ExceptT RpcError SqlLayer) where
  validateAge a = do
    unless (isLegalAge a) $ throwError (rpcError validateUserRpcErrCode (toText $ IllegalUserAge a))
    return $ ValidAge a

addUserMet = toMethod "addUser" f (Required "name" :+: Required "age" :+: ())
  where
    f :: String -> Int -> RpcResult SqlLayer Text
    f uName uAge = do
      createUser (pack uName) uAge
      return "User added"

listUsers = toMethod "getUsers" getAll ()

methods :: [Method SqlLayer]
methods = [addUserMet, listUsers]

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
      liftIO $ putStrLn helpText
      liftIO $ putStrLn "Ready to accept requests"
      forever $ do
        cmd <- liftIO getLine
        liftIO $ when (isExitCommand cmd) exitSuccess
        logInfoN $ mconcat ["RPC call: ", pack cmd]
        r <- call methods (B.pack cmd)
        liftIO (printRpcResult r)
