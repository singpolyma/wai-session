module Main where

import Data.Default (def)
import Data.String (fromString)
import qualified Data.Vault as Vault

import Network.Wai
import Network.Wai.Session (withSession)
import Network.Wai.Session.Map (mapStore_)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (ok200)

main :: IO ()
main = do
	vk <- Vault.newKey
	run 3000 (withSession mapStore_ (fromString "SESSION") def vk (\req -> do
			let Just (sessionLookup, sessionInsert) = Vault.lookup vk (vault req)
			u <- sessionLookup "u"
			sessionInsert "u" (show $ pathInfo req)
			case u of
				Just au ->
					return $ responseLBS ok200 [] (fromString au)
				Nothing ->
					return $ responseLBS ok200 [] (fromString "Nothing")
		))
