module Main(main) where

import System.Directory
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import Control.Monad
import Data.Time
import System.Locale (defaultTimeLocale)

nowStr :: IO String
nowStr = do
    tz <- getCurrentTimeZone
    ct <- getCurrentTime
    return . takeWhile (/= '.') . show $ utcToLocalTime tz ct

createUserTable :: IConnection a => a -> IO ()
createUserTable conn = do
    run conn "CREATE TABLE User (ID INTEGER PRIMARY KEY AUTOINCREMENT, Login TEXT, Email TEXT, Pass TEXT)" []
    run conn "CREATE INDEX UserByLogin ON User (Login)" []
    run conn "CREATE INDEX UserByEmail ON User (Email)" []
    stmt <- prepare conn "INSERT INTO User VALUES (NULL, ?, ?, ?)"
    executeMany stmt
      [ makeVal "demi" "idemura@yandex.ru" "sv32x"
      , makeVal "neil" "flint.emerald@gmail.com" "12345"
      ]
    commit conn
  where
    makeVal :: String -> String -> String -> [SqlValue]
    makeVal name email pass = [toSql name, toSql email, toSql pass]

createPostTable :: IConnection a => a -> IO ()
createPostTable conn = do
    run conn "CREATE TABLE Post (ID INTEGER PRIMARY KEY AUTOINCREMENT, OwnerID INTEGER, Text TEXT, Date TEXT, \
      \FOREIGN KEY (OwnerID) REFERENCES User(ID))" []
    run conn "CREATE INDEX PostByOwnerID ON Post (OwnerID)" []
    run conn "CREATE INDEX PostByDate ON Post (Date)" []
    stmt <- prepare conn "INSERT INTO Post VALUES (NULL, ?, ?, ?)"
    t <- nowStr
    executeMany stmt
      [ makeVal 1 "This is first tweet" t
      , makeVal 2 "Hello Neil!" t
      ]
    commit conn
  where
    makeVal :: Integer -> String -> String -> [SqlValue]
    makeVal owner text date = [toSql owner, toSql text, toSql date]

createWalletTable :: IConnection a => a -> IO ()
createWalletTable conn = do
    let cmd = "CREATE TABLE Wallet (ID INTEGER PRIMARY KEY AUTOINCREMENT, OwnerID INTEGER, Balance INTEGER, Name TEXT, \
      \FOREIGN KEY (OwnerID) REFERENCES User(ID))"
    run conn cmd []
    run conn "CREATE INDEX WalletByOwnerID ON Wallet (OwnerID)" []
    stmt <- prepare conn "INSERT INTO Wallet VALUES (NULL, ?, ?, ?)"
    executeMany stmt
      [ makeVal 1 100 "Private Wallet"
      ]
    commit conn
  where
    makeVal :: Integer -> Integer -> String -> [SqlValue]
    makeVal owner balance name = [toSql owner, toSql balance, toSql name]

createTransactTable :: IConnection a => a -> IO ()
createTransactTable conn = do
    let cmd = "CREATE TABLE Transact (ID INTEGER PRIMARY KEY AUTOINCREMENT, \
      \OwnerID INTEGER, Wallet INTEGER, Date TEXT, Amount INTEGER, Comment TEXT, \
      \FOREIGN KEY (OwnerID) REFERENCES User(ID), \
      \FOREIGN KEY (Wallet) REFERENCES Wallet(ID))"
    run conn cmd []
    run conn "CREATE INDEX TransactByOwnerID ON Transact (OwnerID)" []
    run conn "CREATE INDEX TransactByWallet ON Transact (Wallet)" []
    run conn "CREATE INDEX TransactByDate ON Transact (Date)" []
    stmt <- prepare conn "INSERT INTO Transact VALUES (NULL, ?, ?, ?, ?, ?)"
    t <- nowStr
    executeMany stmt
      [ makeVal 1 1 t (-50) "Lunch"
      ]
    commit conn
  where
    makeVal :: Integer -> Integer -> String -> Integer -> String -> [SqlValue]
    makeVal owner wallet date amount comment = [toSql owner, toSql wallet, toSql date, toSql amount, toSql comment]
    
initDB :: String -> IO ()
initDB file = do
    doesFileExist file >>= \x -> when x (removeFile file)
    conn <- connectSqlite3 file
    createUserTable conn
    createPostTable conn
    createWalletTable conn
    createTransactTable conn
    disconnect conn

main :: IO ()
main = let file = "db.sqlite3" in do
    initDB file
    putStrLn $ file ++ " created."
