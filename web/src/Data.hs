{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module Data
  ( FromRow(..)
  , Post(..)
  , User(..)
  , Wallet(..)
  , Transact(..)
  , dbLatestPosts
  , dbAddPost
  , dbGetUser
  , dbGetUserByID
  , dbGetWallets
  ) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import System.FilePath
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

class FromRow a where
    fromRow :: [SqlValue] -> a

data Post =
     Post
       { postID :: Integer
       , postOwnerID :: Integer
       , postText :: S.ByteString
       , postDate :: S.ByteString
       }

instance FromRow Post where
    fromRow [id, ownerID, text, date] = Post
      (fromSql id)
      (fromSql ownerID)
      (fromSql text)
      (fromSql date)

data User = 
     User
       { userID :: Integer
       , userLogin :: S.ByteString
       , userEmail :: S.ByteString
       , userPassword :: S.ByteString
       }

instance FromRow User where
    fromRow [id, login, email, password] = User
      (fromSql id)
      (fromSql login)
      (fromSql email)
      (fromSql password)

data Wallet = 
     Wallet
       { wltID :: Integer
       , wltOwnerID :: Integer
       , wltBalance :: Integer
       , wltComment :: S.ByteString
       }

instance FromRow Wallet where
    fromRow [id, ownerID, balance, comment] = Wallet
      (fromSql id)
      (fromSql ownerID)
      (fromSql balance)
      (fromSql comment)

data Transact =
     Transact
       { trID :: Integer
       , trOwnerID :: Integer
       , trWallet :: Integer
       , trDate :: S.ByteString
       , trAmount :: Integer
       , trComment :: S.ByteString
       }

instance FromRow Transact where
    fromRow [id, ownerID, wallet, date, amount, comment] = Transact
      (fromSql id)
      (fromSql ownerID)
      (fromSql wallet)
      (fromSql date)
      (fromSql amount)
      (fromSql comment)

dbLatestPosts :: (IConnection c) => c -> IO [Post]
dbLatestPosts conn = do
    rows <- quickQuery conn "SELECT * FROM Post ORDER BY Date DESC" [] -- Lazy fetch
    return $ map fromRow rows

dbAddPost :: (IConnection c) => c -> Post -> IO ()
dbAddPost conn (Post _ ownerID text date) = do
    run conn "INSERT INTO Post VALUES (NULL, ?, ?, ?)" [toSql ownerID, toSql text, toSql date]
    rows <- quickQuery' conn "SELECT last_insert_rowid() FROM Post" []
    print rows
    void $ commit conn

dbGetUser :: (IConnection c) => c -> S.ByteString -> IO (Maybe User)
dbGetUser conn login = do
    rows <- quickQuery' conn "SELECT * FROM User WHERE Login=? ORDER BY Login DESC" [toSql login]
    return $ case rows of
      [row] -> Just $ fromRow row
      _ -> Nothing

dbGetUserByID :: (IConnection c) => c -> Integer -> IO (Maybe User)
dbGetUserByID conn id = do
    rows <- quickQuery' conn "SELECT * FROM User WHERE ID=?" [toSql id]
    return $ case rows of
      [row] -> Just $ fromRow row
      _ -> Nothing

dbGetWallets :: (IConnection c) => c -> User -> IO [Wallet]
dbGetWallets conn u = do
    rows <- quickQuery' conn "SELECT * FROM Wallet WHERE OwnerID=?" [toSql $ userID u]
    return $ map fromRow rows
