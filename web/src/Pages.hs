{-# LANGUAGE OverloadedStrings, PatternGuards, RecordWildCards #-}

module Pages
  ( index
  , newPost
  , login
  , loginCheck
  , logout
  , wallet
  ) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Snap
import Data
import Base
import qualified Text.StringTemplate as T
import Debug.Trace

newPost :: SnapApp L.ByteString
newPost = withAuth $ \u -> do
    Just text <- getPostParam "text"
    t <- getServerTime
    addPost $ Post 0 (userID u) text (showBS t)
    renderMsg "Post added"

authStatus :: SnapApp L.ByteString
authStatus = do
    s <- getSession
    u <- getUserByID (sessionUser s)
    return $ maybe 
      (htmlA "login" "Login")
      (\u -> L.append (L.fromChunks ["Logged in as <b>", userLogin u, "</b>. "]) (htmlA "logout" "Logout"))
      u
    
index :: SnapApp L.ByteString
index = do
    msgs <- getLatestPosts
    text <- fmap (L.intercalate "\n") (mapM renderPost msgs)
    auth <- authStatus
    render "Index" [("text", text), ("auth", auth)]
  where
    renderPost (Post _ ownerID text date) = do
        u <- getUserByID ownerID
        let owner = maybe S.empty userLogin u
        render "Post" [("date", date), ("text", text), ("owner", owner)]

login :: SnapApp L.ByteString
login = do
    s <- getSession
    u <- getUserByID (sessionUser s)
    maybe 
      (render "Login" noAttr)
      (\u -> appError $ S.concat ["You are already signed in as ", userLogin u])
      u

loginCheck :: SnapApp L.ByteString
loginCheck = do
    vals <- mapM getPostParam ["login", "password"]
    case vals of
      [Just l, Just p] -> authAs l p
      _ -> appError "Login form invalid"

  where    
    authAs :: S.ByteString -> S.ByteString -> SnapApp L.ByteString
    authAs login password = do
        getUser login >>= maybe 
          (renderMsg $ S.intercalate " " ["User", login, "not found"])
          (\u -> if password == userPassword u
                   then do
                       modSession $ \s -> s {sessionUser=userID u}
                       renderMsg $ S.concat ["You've been logged in as ", userLogin u]
                   else
                       renderMsg "Wrong password")

logout :: SnapApp L.ByteString
logout = do
    modSession (\s -> s{sessionUser=0})
    renderMsg "Logged out"

wallet :: SnapApp L.ByteString
wallet = withAuth $ \u -> do
    ws <- getWallets u
    if null ws
      then renderMsg "User has not wallets"
      else do
          ts <- getWalletTrans (head ws)
          render "Wallet" [("list", renderList wallet ws)]
  where
    renderList f xs = L.intercalate "\n" $ map f xs
    i2s = S.pack . show
    
    wallet Wallet{..} = let
        tpl = T.newSTMP "<div>Wallet #$id$ <i>$c$</i>\
                        \  <div>Balance: <b>$b$</b></div>\
                        \</div>" :: T.StringTemplate L.ByteString
        in T.render $ T.setManyAttrib [("id", i2s wltID), ("b", i2s wltBalance), ("c", wltComment)] tpl
