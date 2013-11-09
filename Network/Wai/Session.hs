{-# LANGUAGE CPP #-}
module Network.Wai.Session (Session, SessionStore, withSession, genSessionId) where

import Data.Unique (newUnique, hashUnique)
import Data.Ratio (numerator, denominator)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.String (fromString)
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types (ResponseHeaders)
import Network.Wai (Middleware, Request(..), Response(..))
import Web.Cookie (parseCookies, renderSetCookie, SetCookie(..))

#if MIN_VERSION_vault(0,3,0)
import Data.Vault.Lazy (Key)
import qualified Data.Vault.Lazy as Vault
#else
import Data.Vault (Key)
import qualified Data.Vault as Vault
#endif
import Data.ByteString (ByteString)
import qualified Blaze.ByteString.Builder as Builder

-- | Type representing a single session (a lookup, insert pair)
type Session m k v = ((k -> m (Maybe v)), (k -> v -> m ()))

-- | A 'SessionStore' takes in the contents of the cookie (if there was one)
-- and returns a ('Session', 'IO' action to get new contents for cookie) pair
type SessionStore m k v = (Maybe ByteString -> IO (Session m k v, IO ByteString))

-- | Fully parameterised middleware for cookie-based sessions
withSession ::
	SessionStore m k v
	-- ^ The 'SessionStore' to use for sessions
	-> ByteString
	-- ^ Name to use for the session cookie (MUST BE ASCII)
	-> SetCookie
	-- ^ Settings for the cookie (path, expiry, etc)
	-> Key (Session m k v)
	-- ^ 'Data.Vault.Vault' key to use when passing the session through
	-> Middleware
withSession sessions cookieName cookieDefaults vkey app req = do
	(session, getNewCookie) <- lift $ sessions $ lookup cookieName =<< cookies
	resp <- app (req {vault = Vault.insert vkey session (vault req)})
	newCookieVal <- lift getNewCookie
	return $ mapHeader (\hs -> (setCookie, newCookie newCookieVal):hs) resp
	where
	newCookie v = Builder.toByteString $ renderSetCookie $ cookieDefaults {
			setCookieName = cookieName, setCookieValue = v
		}
	cookies = fmap parseCookies $ lookup ciCookie (requestHeaders req)
	setCookie = fromString "Set-Cookie"
	ciCookie = fromString "Cookie"

-- | Simple session ID generator based on time and 'Data.Unique'
--
-- Useful for session stores that use session IDs.
genSessionId :: IO ByteString
genSessionId = do
	u <- fmap (toInteger . hashUnique) newUnique
	time <- fmap toRational getPOSIXTime
	return $ fromString $ show (numerator time * denominator time * u)

mapHeader :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeader f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeader f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapHeader f (ResponseSource s h b) = ResponseSource s (f h) b
