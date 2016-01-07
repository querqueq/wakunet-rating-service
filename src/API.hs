{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module API where

import Control.Monad    
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (ReaderT, runReaderT, lift, MonadReader
                                    , liftIO)
import Control.Monad.Trans.Either   (EitherT, left)
import Network.Wai                  (Application)
import Data.Time                    (UTCTime(..),getCurrentTime)
import Data.Text                    (Text, pack, unpack, split)
import Text.Read                    (readMaybe)
import Data.Time.Format
import Data.Int                     (Int64)
import Database.Persist
import Database.Persist.Types        (PersistValue(..))
import Database.Persist.Sqlite

import Servant
import Servant.Docs

import Config
import Models 
import RatingValue

import Debug.Trace

type CaptureContentType = Capture "contentType" ContentType
type CaptureContentId   = Capture "contentId" ContentId
type SenderId = Header "UserId" UserId
type ContentKeys = QueryParams "contents" ContentKey
type RatingAPI = "ratings" :> SenderId :> 
    (    CaptureContentType :> CaptureContentId :> "like" :> Post '[JSON] ()
    :<|> CaptureContentType :> CaptureContentId :> "dislike" :> Post '[JSON] ()
    :<|> CaptureContentType :> CaptureContentId :> Delete '[JSON] ()
    :<|> CaptureContentType :> CaptureContentId :> Get '[JSON] Ratings
    :<|> ContentKeys :> Get '[JSON] [Ratings]
    ) 

instance ToText ContentKey where
    toText (ContentKey cid ct) = pack $ show cid ++ ":" ++ ct

instance FromText ContentKey where
    fromText x = if length xs == 2
            then ContentKey <$> cid <*> Just ct
            else Nothing
        where xs = split (==':') x
              cid = readMaybe $ unpack $ head xs
              ct = unpack $ head $ tail xs

-- server implementation

type AppM = ReaderT Config (EitherT ServantErr IO)

server :: ServerT RatingAPI AppM
server = ratingServer
    where ratingServer (Just senderId) = 
                rate Like senderId
           :<|> rate Dislike senderId
           :<|> unrate senderId
           :<|> getRatings senderId
           :<|> bulkRatings senderId
          ratingServer Nothing = undefined -- 409 missing header

ratingFilters cid ct = 
    [RatingContentId   ==. cid
    ,RatingContentType ==. ct
    ] 
getRating uid cid ct = selectFirst ((RatingUserId ==. uid) : ratingFilters cid ct)  []

rate :: (MonadIO m, MonadReader Config m) => RatingValue -> UserId -> ContentType -> ContentId -> m ()
rate v uid ct cid = runDb $ do
    rating <- getRating uid cid ct
    case rating of
        -- create rating
        Nothing -> (insert $ Rating cid ct uid v) >> return ()
        -- update rating
        (Just e@(Entity k r)) -> update k [RatingValue =. v]

unrate :: (MonadIO m, MonadReader Config m) => UserId -> ContentType -> ContentId -> m ()
unrate uid ct cid = runDb $ do
    rating <- getRating uid cid ct
    case rating of
        Nothing -> return ()
        (Just e@(Entity k _)) -> delete k

getRatings :: (MonadIO m, MonadReader Config m) => UserId -> ContentType -> ContentId -> m Ratings
getRatings uid ct cid = runDb $ do
    let filters = ratingFilters cid ct
    likes       <- count        ((RatingValue ==. Like)    : filters) 
    dislikes    <- count        ((RatingValue ==. Dislike) : filters) 
    usersRating <- selectFirst  ((RatingUserId ==. uid)    : filters) []
    return $ Ratings likes dislikes (ContentKey cid ct) uid $ usersRating >>= (\(Entity _ v) -> return $ ratingValue v)

bulkRatings :: (MonadIO m, MonadReader Config m) => UserId -> [ContentKey] -> m [Ratings]
bulkRatings uid cks = trace (show cks) $ mapM (\(ContentKey cid ct) -> getRatings uid ct cid) cks
    

ratingAPI :: Proxy RatingAPI
ratingAPI = Proxy

app :: Config -> Application
app cfg = serve ratingAPI (readerServer cfg)

readerServer :: Config -> Server RatingAPI
readerServer cfg = enter (readerToEither cfg) server
 
readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

apiDocs :: API
apiDocs = docs ratingAPI
updateDocs = writeFile "rating-service.md" $ markdown apiDocs

instance ToParam ContentKeys where
    toParam _ = DocQueryParam 
                    "contents"
                    (map (unpack . toText) sampleContentKeys)
                    "List of content keys, e.g. ratings?contents[]=1:post&contents[]=3:event&contents[]=5:post"
                    List
                where sampleContentKeys =
                        [ContentKey 1 "post"
                        ,ContentKey 3 "event"
                        ,ContentKey 5 "post"
                        ]

instance ToCapture CaptureContentType where
    toCapture _ = DocCapture "contentType" "(string) super type of rated content"
    
instance ToCapture CaptureContentId where
    toCapture _ = DocCapture "contentId" "(long) id if rated content"

instance ToSample () () where
    toSample _ = Nothing
