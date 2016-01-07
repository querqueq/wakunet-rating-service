{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Aeson                  -- (ToJSON, FromJSON)
import GHC.Generics                (Generic)
import Control.Monad.Reader        (ReaderT, asks, liftIO)
import Database.Persist.Sqlite     (SqlBackend(..),runMigration
                                   ,runSqlPool,fromSqlKey,Entity(..))
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)
import Data.Time
import Data.Text
import Data.Int                    (Int64,Int)

import Servant.Docs

import Config
import RatingValue

type Id = Int64
type ContentType = String
type ContentId = Id
type UserId = Id

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Rating json
    contentId ContentId
    contentType ContentType
    userId UserId
    value RatingValue
--    created UTCTime
    UniqueRating contentId contentType userId
    deriving Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

data ContentKey = ContentKey
    { contentId     :: ContentId
    , contentType   :: ContentType
    } deriving (Show, Generic, Eq)

instance ToJSON ContentKey
instance FromJSON ContentKey

data BulkRequest = BulkRequest
    { request :: [ContentKey]
    } deriving (Show, Generic, Eq)

instance FromJSON BulkRequest
instance ToJSON BulkRequest

data BulkResponse = BulkResponse
    { ratings :: [(ContentType, [(ContentId, Ratings)])]
    } deriving (Show, Generic, Eq)

instance ToJSON BulkResponse

{--
type ContentKey = (ContentId,ContentType)

instance ToJSON ContentKey where
    toJSON (cid, ct) = object ["contentId" .= cid, "contentType" .= ct]

instance FromJSON ContentKey where
    parseJSON (Object k) = (,) <$> k .: "contentId" <*> k .: "contentType"
--}
--
data Ratings = Ratings
    { likes         :: Int
    , dislikes      :: Int
    , contentKey    :: ContentKey
    , userId        :: Id
    , userRating    :: Maybe RatingValue
    } deriving (Show,Eq,Generic)

instance ToJSON Ratings

instance ToSample Rating Rating where
    toSample _ = Just $ sampleRating 1

instance ToSample Ratings Ratings where
    toSample _ = Just $ sampleRatings

instance ToSample BulkRequest BulkRequest where
    toSamples _ =
        [ ("Requesting ratings for a post and an event", BulkRequest 
            [ ContentKey 13 "post"
            , ContentKey 5 "event"
            , ContentKey 1 "post"
            ]
          )
        ]

instance ToSample [Ratings] [Ratings] where
    toSample _ = Just $ [sampleRatings, sampleRatingsNoLikes 3 "event", sampleRatingsNoLikes 5 "post"]

instance ToSample ContentKey ContentKey where
    toSample _ = Just $ ContentKey 13 "post"

instance ToSample [ContentKey] [ContentKey] where
    toSample _ = Just [ContentKey 13 "post",ContentKey 4 "event",ContentKey 1 "post"]

sampleTime n = (UTCTime (fromGregorian 2015 12 28) (60*60*10+n*10))
sampleRating 1 = Rating 13 "post" 1 Like -- $ sampleTime 0
sampleRating 2 = Rating 13 "post" 2 Dislike -- $ sampleTime 1
sampleRating 3 = Rating 13 "post" 3 Like -- $ sampleTime 2
sampleRatings = Ratings 2 1 (ContentKey 13 "post") 2 $ Just Dislike
sampleRatingsNoLikes id ctype = Ratings 0 0 (ContentKey id ctype) 2 Nothing
