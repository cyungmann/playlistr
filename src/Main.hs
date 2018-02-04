{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Unsafe              #-}

module Main where

import Control.Applicative
  ( empty
  )
import Control.Exception.Base
  ( bracket
  )
import Control.Monad.Except
  ( ExceptT(ExceptT)
  , liftIO
  , runExceptT
  )
import Data.Aeson
  ( FromJSON
  , Value(Object)
  , camelTo2
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , parseJSON
  , (.:)
  )
import Data.Proxy
  ( Proxy(Proxy)
  )
import Data.Semigroup
  ( (<>)
  )
import Data.Text
  ( Text
  , pack
  , replace
  , unpack
  )
import Data.Text.Encoding
  ( encodeUtf8
  )
import Data.Yaml.Config
  ( loadYamlSettingsArgs
  , useEnv
  )
import Database.PostgreSQL.Simple
  ( close
  , connect
  , connectPassword
  , connectUser
  , defaultConnectInfo
  )
import Debug.Trace
  ( traceShowM
  )
import GHC.Generics
  ( Generic
  )
import Network.HTTP.Client.TLS
  ( newTlsManager
  )
import Prelude
  ( Bool
  , Eq
  , IO
  , Int
  , Maybe(Just)
  , Show
  , String
  , either
  , putStrLn
  , ($)
  , (.)
  , (<$>)
  , (<*>)
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
import Text.Show.Pretty
  ( pPrint
  )
import Web.FormUrlEncoded
  ( ToForm
  , toForm
  )

data Configuration = Configuration
  { _configurationSpotifyClientId     :: Text
  , _configurationSpotifyClientSecret :: Text
  , _configurationDatabaseUser        :: String
  , _configurationDatabasePassword    :: String
  } deriving (Eq, Show)

instance FromJSON Configuration where
  parseJSON (Object o) =
    Configuration <$> o .: "spotify-client-id" <*> o .: "spotify-client-secret" <*>
    o .: "database-user" <*>
    o .: "database-password"
  parseJSON _ = empty

newtype TokenRequest = TokenRequest
  { _tokenRequestGrantType :: Text
  } deriving (Eq, Show)

instance ToForm TokenRequest where
  toForm tokenRequest =
    [("grant_type", toQueryParam (_tokenRequestGrantType tokenRequest))]

data TokenResponse = TokenResponse
  { _tokenResponseAccessToken :: Text
  , _tokenResponseTokenType   :: Text
  , _tokenResponseExpiresIn   :: Int
  } deriving (Eq, Generic, Show)

instance FromJSON TokenResponse where
  parseJSON =
    genericParseJSON $
    defaultOptions
      { fieldLabelModifier =
          camelTo2 '_' . unpack . replace "_tokenResponse" "" . pack
      }

data Paging a = Paging
  { _pagingHref     :: Text
  , _pagingItems    :: [a]
  , _pagingLimit    :: Int
  , _pagingNext     :: Maybe Text
  , _pagingOffset   :: Int
  , _pagingPrevious :: Maybe Text
  , _pagingTotal    :: Int
  } deriving (Eq, Generic, Show)

instance (FromJSON a) => FromJSON (Paging a) where
  parseJSON =
    genericParseJSON $
    defaultOptions
      {fieldLabelModifier = camelTo2 '_' . unpack . replace "_paging" "" . pack}

data SimplifiedPlaylist = SimplifiedPlaylist
  { _simplifiedPlaylistCollaborative :: Bool
  --, _simplifiedPlaylistExternalUrls :: ExternalUrl
  , _simplifiedPlaylistHref          :: Text
  , _simplifiedPlaylistId            :: Text
  --, _simplifiedPlaylistImages :: [Image]
  , _simplifiedPlaylistName          :: Text
  --, _simplifiedPlaylistOwner :: User
  , _simplifiedPlaylistPublic        :: Maybe Bool
  , _simplifiedPlaylistSnapshotId    :: Text
  --, _simplifiedPlaylistTracks :: Tracks
  , _simplifiedPlaylistType          :: Text
  , _simplifiedPlaylistUri           :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON SimplifiedPlaylist where
  parseJSON =
    genericParseJSON $
    defaultOptions
      { fieldLabelModifier =
          camelTo2 '_' . unpack . replace "_simplifiedPlaylist" "" . pack
      }

data FeaturedPlaylistsResponse = FeaturedPlaylistsResponse
  { _featuredPlaylistsResponseMessage   :: Text
  , _featuredPlaylistsResponsePlaylists :: Paging SimplifiedPlaylist
  } deriving (Eq, Show)

instance FromJSON FeaturedPlaylistsResponse where
  parseJSON (Object o) =
    FeaturedPlaylistsResponse <$> o .: "message" <*> o .: "playlists"
  parseJSON _ = empty

newtype Authorization = Authorization
  { _authorizationAccessToken :: Text
  } deriving (Eq, Show)

instance ToHttpApiData Authorization where
  toQueryParam authorization =
    "Bearer " <> _authorizationAccessToken authorization

type SpotifyAccountsApi
   = "token" :> BasicAuth "spotify" () :> ReqBody '[ FormUrlEncoded] TokenRequest :> Post '[ JSON] TokenResponse

type SpotifyApi
   = "browse" :> "featured-playlists" :> Header "Authorization" Authorization :> Get '[ JSON] FeaturedPlaylistsResponse

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
  pPrint config
  bracket
    (connect
       defaultConnectInfo
         { connectUser = _configurationDatabaseUser config
         , connectPassword = _configurationDatabasePassword config
         })
    close
    (\_ -> pPrint ("yay!" :: Text))
  manager <- newTlsManager
  result <-
    runExceptT $ do
      tokenResponse <-
        ExceptT . liftIO $
        runClientM
          (queries
             (_configurationSpotifyClientId config)
             (_configurationSpotifyClientSecret config))
          (ClientEnv manager (BaseUrl Https "accounts.spotify.com" 443 "api"))
      liftIO . pPrint $ tokenResponse
      ExceptT . liftIO $
        runClientM
          (queries' (_tokenResponseAccessToken tokenResponse))
          (ClientEnv manager (BaseUrl Https "api.spotify.com" 443 "v1"))
  either pPrint pPrint result
