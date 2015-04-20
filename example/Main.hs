module Main where

import Data.Default (def)
import Data.String (fromString)
import qualified Data.Vault.Lazy as Vault

import Network.Wai
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.Map (mapStore_)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (ok200)

app :: Vault.Key (Session IO String String) -> Application
app session env = (>>=) $ do
	u <- sessionLookup "u"
	sessionInsert "u" insertThis
	return $ responseLBS ok200 [] $ maybe (fromString "Nothing") fromString u
	where
	insertThis = show $ pathInfo env
	Just (sessionLookup, sessionInsert) = Vault.lookup session (vault env)

main :: IO ()
main = do
	session <- Vault.newKey
	store <- mapStore_
	run 3000 $ withSession store (fromString "SESSION") def session $ app session
