{-# LANGUAGE OverloadedStrings #-}

module Config where

import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)
import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)

import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, ConnectionString)
--import Database.Persist.MySQL --(ConnectionPool, createMySQLPool, ConnectInfo)
import Database.Persist.Sqlite (createSqlitePool, ConnectionPool)

import Heroku (herokuConnStr)

data Config = Config 
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    }

data Environment = 
    Development
  | Test
  | Production
  | Heroku
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config
    { getPool = undefined
    , getEnv  = Development
    }

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger _ = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT $ createSqlitePool ":memory:" 20 
makePool Development = runStdoutLoggingT $ createSqlitePool "mdc-post-prototype-dev.db" 20
makePool Heroku = do
    connStr <- herokuConnStr
    pool <- runNoLoggingT $ createPostgresqlPool connStr (envPool Test)
    return pool
--makePool Production = runStdoutLoggingT $ createMySQLPool (connInfo Production) (envPool Production)

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8
{--
connInfo :: Environment -> ConnectInfo
connInfo _ = ConnectInfo "127.0.0.1" 3306 "root" "pw" "mdc-post-prototype" [] "" Nothing
--}
