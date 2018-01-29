{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Unsafe #-}

module Main where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, Value(Object), (.:), parseJSON)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Debug.Trace (traceShowM)
import Network.HTTP.Client.TLS (newTlsManager)
import Prelude
  ( Either(Left, Right)
  , Eq
  , IO
  , Int
  , Maybe(Just)
  , Show
  , ($)
  , (<$>)
  , (<*>)
  , print
  , pure
  , putStrLn
  , show
  )
import Servant.API
  ( (:>)
  , BasicAuth
  , BasicAuthData(BasicAuthData)
  , FormUrlEncoded
  , Get
  , Header
  , JSON
  , Post
  , ReqBody
  , ToHttpApiData
  , toQueryParam
  )
import Servant.Client
  ( BaseUrl(BaseUrl)
  , ClientEnv(ClientEnv)
  , ClientM
  , Scheme(Https)
  , client
  , runClientM
  )
import Web.FormUrlEncoded (ToForm, toForm)

data Configuration = Configuration
  { _configurationSpotifyClientId :: Text
  , _configurationSpotifyClientSecret :: Text
  } deriving (Eq, Show)

instance FromJSON Configuration where
  parseJSON (Object o) =
    Configuration <$> o .: "spotify-client-id" <*> o .: "spotify-client-secret"
  parseJSON _ = empty

newtype TokenRequest = TokenRequest
  { _tokenRequestGrantType :: Text
  } deriving (Eq, Show)

instance ToForm TokenRequest where
  toForm tokenRequest =
    [("grant_type", toQueryParam (_tokenRequestGrantType tokenRequest))]

data TokenResponse = TokenResponse
  { _tokenResponseAccessToken :: Text
  , _tokenResponseTokenType :: Text
  , _tokenResponseExpiresIn :: Int
  } deriving (Eq, Show)

instance FromJSON TokenResponse where
  parseJSON (Object o) =
    TokenResponse <$> o .: "access_token" <*> o .: "token_type" <*>
    o .: "expires_in"
  parseJSON _ = empty

data FeaturedPlaylistsResponse =
  FeaturedPlaylistsResponse
  deriving (Eq, Show)

instance FromJSON FeaturedPlaylistsResponse where
  parseJSON _ = pure FeaturedPlaylistsResponse

newtype Authorization = Authorization
  { _authorizationAccessToken :: Text
  } deriving (Eq, Show)

instance ToHttpApiData Authorization where
  toQueryParam authorization =
    "Bearer " <> _authorizationAccessToken authorization

type SpotifyAccountsApi
   = "token" :> BasicAuth "spotify" () :> ReqBody '[ FormUrlEncoded] TokenRequest :> Post '[JSON] TokenResponse

type SpotifyApi
   = "browse" :> "featured-playlists" :> Header "Authorization" Authorization :> Get '[JSON] FeaturedPlaylistsResponse

spotifyAccountsApi :: Proxy SpotifyAccountsApi
spotifyAccountsApi = Proxy

spotifyApi :: Proxy SpotifyApi
spotifyApi = Proxy

getToken :: BasicAuthData -> TokenRequest -> ClientM TokenResponse
getToken = client spotifyAccountsApi

getFeaturedPlaylists :: Maybe Authorization -> ClientM FeaturedPlaylistsResponse
getFeaturedPlaylists = client spotifyApi

queries :: Text -> Text -> ClientM TokenResponse
queries spotifyClientId spotifyClientSecret =
  getToken
    (BasicAuthData (encodeUtf8 spotifyClientId) (encodeUtf8 spotifyClientSecret))
    (TokenRequest "client_credentials")

queries' :: Text -> ClientM FeaturedPlaylistsResponse
queries' accessToken = do
  traceShowM accessToken
  getFeaturedPlaylists $ Just (Authorization accessToken)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  config :: Configuration <- loadYamlSettingsArgs [] useEnv
  print config
  manager <- newTlsManager
  res <-
    runClientM
      (queries
         (_configurationSpotifyClientId config)
         (_configurationSpotifyClientSecret config))
      (ClientEnv manager (BaseUrl Https "accounts.spotify.com" 443 "api"))
  case res of
    Left err -> showError err
    Right tokenResponse -> do
      print tokenResponse
      res' <- runClientM (queries' (_tokenResponseAccessToken tokenResponse)) (ClientEnv manager (BaseUrl Https "api.spotify.com" 443 "v1"))
      case res' of
        Left err -> showError err
        Right featuredPlaylistsResponse -> print featuredPlaylistsResponse
  where
    showError :: Show a => a -> IO ()
    showError err = putStrLn $ "Error: " <> show err
