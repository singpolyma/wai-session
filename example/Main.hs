module Main where

import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Vault.Lazy as Vault

import Network.Wai (Application, pathInfo, responseLBS, vault)
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.Map (mapStore_)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (ok200)
import Web.Cookie (defaultSetCookie)

app :: Vault.Key (Session IO String String) -> Application
app vaultKey request respond = do
    u <- sessionLookup "u"
    sessionInsert "u" path
    respond . responseLBS ok200 [] . fromString $ fromMaybe "Nothing" u;
    where
        path = show $ pathInfo request
        Just (sessionLookup, sessionInsert) = Vault.lookup vaultKey (vault request)

main :: IO ()
main = do
    vaultKey <- Vault.newKey
    store <- mapStore_
    run 3000 . withSession store (fromString "SESSION") defaultSetCookie vaultKey $ app vaultKey
