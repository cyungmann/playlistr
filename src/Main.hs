{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE Unsafe                     #-}

module Main where

import           Control.Applicative
  ( empty
  )
import           Control.Exception.Base
  ( bracket
  )
import           Control.Monad.Except
  ( ExceptT(ExceptT)
  , liftIO
  , runExceptT
  )
import           Data.Aeson
  ( FromJSON
  , GFromJSON
  , Value(Object)
  , Zero
  , camelTo2
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , parseJSON
  , (.:)
  )
import           Data.Aeson.Types
  ( Parser
  )
import qualified Data.Map.Strict            as Map
  ( Map
  )
import           Data.Proxy
  ( Proxy(Proxy)
  )
import           Data.Semigroup
  ( (<>)
  )
import           Data.Text
  ( Text
  , pack
  , replace
  , unpack
  )
import           Data.Text.Encoding
  ( encodeUtf8
  )
import           Data.Time
  ( NominalDiffTime
  , UTCTime
  )
import           Data.Yaml.Config
  ( loadYamlSettingsArgs
  , useEnv
  )
import           Database.PostgreSQL.Simple
  ( close
  , connect
  , connectPassword
  , connectUser
  , defaultConnectInfo
  )
import           Debug.Trace
  ( traceShowM
  )
import           GHC.Generics
  ( Generic
  , Rep
  )
import           Network.HTTP.Client.TLS
  ( newTlsManager
  )
import           Prelude
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
import           Servant.API
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
import           Servant.Client
  ( BaseUrl(BaseUrl)
  , ClientEnv(ClientEnv)
  , ClientM
  , Scheme(Https)
  , client
  , runClientM
  )
import           Text.Show.Pretty
  ( pPrint
  )
import           Web.FormUrlEncoded
  ( ToForm
  , toForm
  )

parseJSON' :: (Generic a, GFromJSON Zero (Rep a)) => Text -> Value -> Parser a
parseJSON' prefix =
  genericParseJSON $
  defaultOptions
    {fieldLabelModifier = camelTo2 '_' . unpack . replace prefix "" . pack}

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
  parseJSON = parseJSON' "_tokenResponse"

data Paging a = Paging
  { _pagingHref     :: Text
  , _pagingItems    :: Maybe [a]
  , _pagingLimit    :: Maybe Int
  , _pagingNext     :: Maybe Text
  , _pagingOffset   :: Maybe Int
  , _pagingPrevious :: Maybe Text
  , _pagingTotal    :: Int
  } deriving (Eq, Generic, Show)

instance (FromJSON a) => FromJSON (Paging a) where
  parseJSON = parseJSON' "_paging"

newtype ExternalUrl =
  ExternalUrl (Map.Map Text Text)
  deriving (Eq, FromJSON, Generic, Show)

data Followers = Followers
  { _followersHref  :: Maybe Text
  , _followersTotal :: Int
  } deriving (Eq, Generic, Show)

instance FromJSON Followers where
  parseJSON = parseJSON' "_followers"

data Image = Image
  { _imageHeight :: Maybe Int
  , _imageUrl    :: Text
  , _imageWidth  :: Maybe Int
  } deriving (Eq, Generic, Show)

instance FromJSON Image where
  parseJSON = parseJSON' "_image"

data User = User
  { _userDisplayName  :: Text
  , _userExternalUrls :: ExternalUrl
  , _userFollowers    :: Maybe Followers
  , _userHref         :: Text
  , _userId           :: Text
  , _userImages       :: Maybe [Image]
  , _userType         :: String
  , _userUri          :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON User where
  parseJSON = parseJSON' "_user"

data SimplifiedArtist = SimplifiedArtist
  { _simplifiedArtistExternalUrls :: ExternalUrl
  , _simplifiedArtistHref         :: Text
  , _simplifiedArtistId           :: Text
  , _simplifiedArtistName         :: Text
  , _simplifiedArtistType         :: Text
  , _simplifiedArtistUri          :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON SimplifiedArtist where
  parseJSON = parseJSON' "_simplifiedArtist"

data SimplifiedAlbum = SimplifiedAlbum
  { _simplifiedAlbumAlbumType        :: Text
  , _simplifiedAlbumArtists          :: [SimplifiedArtist]
  , _simplifiedAlbumAvailableMarkets :: [Text]
  , _simplifiedAlbumExternalUrls     :: ExternalUrl
  , _simplifiedAlbumHref             :: Text
  , _simplifiedAlbumId               :: Text
  , _simplifiedAlbumImages           :: [Image]
  , _simplifiedAlbumName             :: Text
  , _simplifiedAlbumType             :: Text
  , _simplifiedAlbumUri              :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON SimplifiedAlbum where
  parseJSON = parseJSON' "_simplifiedAlbum"

newtype ExternalId =
  ExternalId (Map.Map Text Text)
  deriving (Eq, FromJSON, Generic, Show)

data LinkedTrack = LinkedTrack
  { _linkedTrackExternalUrls :: ExternalUrl
  , _linkedTrackHref         :: Text
  , _linkedTrackId           :: Text
  , _linkedTrackType         :: Text
  , _linkedTrackUri          :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON LinkedTrack where
  parseJSON = parseJSON' "_linked"

data Restrictions = Restrictions
  { _restrictionsReason :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Restrictions where
  parseJSON = parseJSON' "_restrictions"

data Track = Track
  { _trackAlbum            :: SimplifiedAlbum
  , _trackArtists          :: [SimplifiedArtist]
  , _trackAvailableMarkets :: [Text]
  , _trackDiscNumber       :: Int
  , _trackDuration         :: NominalDiffTime
  , _trackExplicit         :: Bool
  , _trackExternalIds      :: ExternalId
  , _trackExternalHrefs    :: ExternalUrl
  , _trackHref             :: Text
  , _trackId               :: Text
  , _trackIsPlayable       :: Bool
  , _trackLinkedFrom       :: LinkedTrack
  , _trackRestrictions     :: Restrictions
  , _trackName             :: Text
  , _trackPopularity       :: Int
  , _trackPreviewUrl       :: Text
  , _trackTrackNumber      :: Int
  , _trackType             :: Text
  , _trackUri              :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Track where
  parseJSON = parseJSON' "_track"

data PlaylistTrack = PlaylistTrack
  { _playlistTrackAddedAt :: UTCTime
  , _playlistTrackAddedBy :: User
  , _playlistTrackIsLocal :: Bool
  , _playlistTrackTrack   :: Track
  } deriving (Eq, Generic, Show)

data Playlist = Playlist
  { _playlistCollaborative :: Bool
  , _playlistDescription   :: Text
  , _playlistExternalUrls  :: ExternalUrl
  , _playlistFollowers     :: Followers
  , _playlistHref          :: Text
  , _playlistId            :: Text
  , _playlistImages        :: [Image]
  , _playlistName          :: Text
  , _playlistOwner         :: User
  , _playlistPublic        :: Maybe Bool
  , _playlistSnapshotId    :: Text
  , _playlistTracks        :: Paging PlaylistTrack
  , _playlistType          :: Text
  , _playlistUri           :: Text
  } deriving (Eq, Generic, Show)

data SimplifiedPlaylist = SimplifiedPlaylist
  { _simplifiedPlaylistCollaborative :: Bool
  , _simplifiedPlaylistExternalUrls  :: ExternalUrl
  , _simplifiedPlaylistHref          :: Text
  , _simplifiedPlaylistId            :: Text
  , _simplifiedPlaylistImages        :: [Image]
  , _simplifiedPlaylistName          :: Text
  , _simplifiedPlaylistOwner         :: User
  , _simplifiedPlaylistPublic        :: Maybe Bool
  , _simplifiedPlaylistSnapshotId    :: Text
  , _simplifiedPlaylistTracks        :: Paging Track
  , _simplifiedPlaylistType          :: Text
  , _simplifiedPlaylistUri           :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON SimplifiedPlaylist where
  parseJSON = parseJSON' "_simplifiedPlaylist"

data FeaturedPlaylistsResponse = FeaturedPlaylistsResponse
  { _featuredPlaylistsResponseMessage   :: Text
  , _featuredPlaylistsResponsePlaylists :: Paging SimplifiedPlaylist
  } deriving (Eq, Generic, Show)

instance FromJSON FeaturedPlaylistsResponse where
  parseJSON = parseJSON' "_featuredPlaylistsResponse"

newtype Authorization = Authorization
  { _authorizationAccessToken :: Text
  } deriving (Eq, Generic, Show)

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
