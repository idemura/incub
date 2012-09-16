{-# LANGUAGE OverloadedStrings, PatternGuards, ExistentialQuantification, GeneralizedNewtypeDeriving, Rank2Types #-}

module Base
  ( ServerTime
  , SnapApp
  , Session(..)
  , ShowBS(..)
  , ReadBS(..)
  , TplSrc(..)
  , getServerTime
  , noAttr
  , render
  , renderMsg
  , appError
  , digest
  , runApp
  , addPost
  , getUser
  , getUserByID
  , getLatestPosts
  , getWallets
  , getWalletTrans
  , htmlA
  , getSession
  , setSession
  , modSession
  , withAuth
  ) where

import Prelude hiding (catch)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative
import Control.Monad.CatchIO
import Data.List
import Data.Char
import Data.Time
import System.Locale (defaultTimeLocale)
import System.FilePath
import qualified System.IO.Error as IO
import qualified System.CPUTime
import qualified Text.StringTemplate as T
import qualified Crypto.Hash.SHA1
import qualified Numeric
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import Debug.Trace
import Snap
import Data


class ShowBS a where
    showBS :: a -> S.ByteString

class ReadBS a where
    readBS :: S.ByteString -> Maybe a

data TplSrc =
     TplStr L.ByteString
   | TplFile String

data Session =
     Session
       { sessionHash :: S.ByteString
       , sessionUser :: Integer
       }

data AppState =
     forall a. IConnection a => AppState
       { appSession :: Session
       , appDB :: a
       }
   | EmptyState

newtype ServerTime = ServerTime LocalTime

instance ShowBS ServerTime where
    showBS (ServerTime t) = S.pack . takeWhile (/= '.') . show $ t

newtype SnapApp a = SnapApp { fromSnapApp :: StateT AppState Snap a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadCatchIO, MonadSnap)

getState :: SnapApp AppState
getState = SnapApp get

setState :: AppState -> SnapApp ()
setState = SnapApp . put

getSession :: SnapApp Session
getSession = appSession <$> getState

setSession :: Session -> SnapApp ()
setSession s = do
    e <- getState
    setState $ e {appSession=s}
    writeSession s

modSession :: (Session -> Session) -> SnapApp ()
modSession f = getState >>= setSession . f . appSession

withDB :: (forall a. (IConnection a) => a -> IO b) -> SnapApp b
withDB mf = do
    AppState {appDB=c} <- getState
    liftIO $ mf c

getUser :: S.ByteString -> SnapApp (Maybe User)
getUser s = withDB $ \c ->
    dbGetUser c s

getUserByID :: Integer -> SnapApp (Maybe User)
getUserByID i = withDB $ \c ->
    dbGetUserByID c i

addPost :: Post -> SnapApp ()
addPost p = withDB $ \c ->
    dbAddPost c p

getLatestPosts :: SnapApp [Post]
getLatestPosts = withDB $ \c ->
    dbLatestPosts c

getWallets :: User -> SnapApp [Wallet]
getWallets u = withDB $ \c ->
    dbGetWallets c u

getWalletTrans :: Wallet -> SnapApp [Transact]
getWalletTrans w = withDB $ \c ->
    map fromRow <$> quickQuery' c "SELECT * FROM Transact WHERE Wallet=?" [toSql $ wltID w]

getServerTime :: (MonadIO m) => m ServerTime
getServerTime = liftIO $ do
    tz <- getCurrentTimeZone
    ct <- getCurrentTime
    return $ ServerTime $ utcToLocalTime tz ct

noAttr :: [(String, S.ByteString)]
noAttr = []

render :: (MonadIO m, T.ToSElem s) => String -> [(String, s)] -> m L.ByteString
render = let
    -- Read files once.
    group = T.directoryGroupLazy "templates"
    in \n a -> liftIO $ do
                 g <- group
                 return $ maybe L.empty (T.render . T.setManyAttrib a) (T.getStringTemplate n g)

appError :: S.ByteString -> SnapApp L.ByteString
appError text = do
    logError text
    render "Msg" [("h", "Application Error"), ("t", L.fromChunks [text])]

renderMsg :: (MonadIO m) => S.ByteString -> m L.ByteString
renderMsg text = render "Msg" [("h", L.empty), ("t", L.fromChunks [text])]

instance ShowBS Session where
    showBS (Session hash user) = S.unlines [hash, S.pack . show $ user]

instance ReadBS Session where
    readBS s = case S.lines s of
        [hash, user] -> Just $ Session hash (read . S.unpack $ user)
        _ -> Nothing

sessionFile :: String -> String
sessionFile hash = joinPath ["env", "sessions", hash]

writeSession :: (MonadIO m) => Session -> m ()
writeSession s = liftIO $ do
    S.writeFile (sessionFile . S.unpack . sessionHash $ s) (showBS s)

withAuth :: (User -> SnapApp L.ByteString) -> SnapApp L.ByteString
withAuth h = do
    s <- getSession
    u <- getUserByID (sessionUser s)
    maybe (appError "Authorized access only") h u
    
runApp :: SnapApp L.ByteString -> Snap ()
runApp app = do
    r <- evalStateT (fromSnapApp $ go app) EmptyState
    writeLBS r

  where
    go :: SnapApp L.ByteString -> SnapApp L.ByteString
    go app = do
        s <- getSession
        c <- liftIO $ connectSqlite3 $ "env" </> "db.sqlite3"
        r <- SnapApp $ do
               put $ AppState {appSession=s, appDB=c}
               fromSnapApp app
        liftIO $ disconnect c
        return r

    newSession :: SnapApp Session
    newSession = do
        time <- getServerTime
        tick <- liftIO System.CPUTime.getCPUTime
        let hash = digest $ S.concat ["demi ", S.pack $ Numeric.showHex tick " ", showBS time]
        modifyResponse $ addResponseCookie $ Cookie "session" hash Nothing Nothing Nothing False False
        return $ Session hash 0

    getSession :: SnapApp Session
    getSession = let
        readSession hash = do
            mbs <- liftIO $ IO.catch
              (do{ t <- S.readFile (sessionFile (S.unpack hash)); return $ Just t })
              (\_ -> return Nothing)
            case mbs of
              Just s | Just session <- readBS s -> return session
              _ -> newSession
        in getCookie "session" >>= maybe newSession (readSession . cookieValue)

digest :: S.ByteString -> S.ByteString
digest = hex . Crypto.Hash.SHA1.hash
  where
    hex s = let
        fn c str = let
            n = ord c
            s = Numeric.showHex n str
            in if n < 16 then '0' : s else s
        in S.pack $ S.foldr fn [] s

htmlA :: L.ByteString -> L.ByteString -> L.ByteString
htmlA ref inner = L.concat ["<a href=\"/", ref, "\">", inner, "</a>"]
