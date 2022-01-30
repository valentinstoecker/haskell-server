{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module User where

import           Control.Monad.IO.Class         ( liftIO )
import           Database.Persist               ( PersistStoreWrite(insert) )
import           Database.Persist.MySQL         ( BackendKey(SqlBackendKey)
                                                , ConnectInfo
                                                  ( connectDatabase
                                                  , connectHost
                                                  , connectPassword
                                                  , connectUser
                                                  )
                                                , defaultConnectInfo
                                                , openMySQLConn
                                                , runSqlConn
                                                )
import           Database.Persist.TH            ( mkPersist
                                                , persistLowerCase
                                                , sqlSettings
                                                )

import           Data.Text                      ( Text )

mkPersist sqlSettings [persistLowerCase|
Person
    username Text
    displayname Text
    password Text
    UserName username
    deriving Show
|]

main = do
  (conn, backend) <- openMySQLConn
    (defaultConnectInfo { connectUser     = "user"
                        , connectPassword = "password"
                        , connectHost     = "127.0.0.1"
                        , connectDatabase = "testserver"
                        }
    )
    (\_ _ _ _ -> return ())
  runSqlConn
    (do
      insert $ Person "valentin" "Valle" "password"
    )
    backend
  putStrLn "Done"
