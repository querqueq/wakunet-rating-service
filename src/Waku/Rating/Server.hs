{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Waku.Rating.Server where

import Control.Monad
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (ReaderT, runReaderT, lift, MonadReader
                                    , liftIO)
import Control.Monad.Trans.Either   (EitherT, left, right)
import Network.Wai                  (Application)
import Data.Time                    (UTCTime(..),getCurrentTime)
import Data.Text                    (Text, pack, unpack, split)
import Text.Read                    (readMaybe)
import Data.Time.Format
import Data.Int                     (Int64)
import Database.Persist
import Database.Persist.Types        (PersistValue(..))
import Database.Persist.Sqlite
import Data.Maybe                   (isJust)

import Servant
import Servant.Docs

import Config
import Waku.Rating.Database
import Waku.Rating.RatingValue
import Waku.Models.Rating
import Waku.Models.General
import Waku.APIs.RatingAPI
import Waku.APIs.StickyAPI
import Waku.Clients.GroupClient
import Waku.Servers.Util            (require)
import Waku.Servers.Errors

type AppM = ReaderT Config (EitherT ServantErr IO)

errNoSenderId = err409 { errReasonPhrase = "No sender id", errBody = "No sender id" }

-- Sticky ist falsch implementiert
-- Siehe use case

server :: ServerT RatingAPI AppM
server = ratingServer 
    where ratingServer (Just senderId) =
                rate Like senderId
           :<|> rate Dislike senderId
           :<|> remove senderId
           :<|> getRatings senderId
           :<|> bulkRatings senderId
          ratingServer Nothing = error "No sender id"

--value :: Entity a -> Maybe a
value :: Functor f => f (Entity b) -> f b
value = fmap (\(Entity _ v) -> v)
ratingFilters cid ct =
    [RatingContentId   ==. cid
    ,RatingContentType ==. ct
    ]
getRating uid cid ct = selectFirst ((RatingUserId ==. uid) : ratingFilters cid ct)  []

{--
stick :: (MonadIO m, MonadReader Config m) => Bool -> Id -> ContentType -> ContentId -> m ()
--stick v Nothing _ _ = errNoSenderId
stick v uid ct cid = runDb $ do
    rating <- getRating uid cid ct
    case rating of
        -- create rating
        Nothing -> (insert $ Rating cid ct uid Nothing v) >> return ()
        -- update rating
        (Just e@(Entity k r)) -> update k [RatingSticky =. v]
--}

rate :: (MonadIO m, MonadReader Config m) => RatingValue -> Id -> ContentType -> ContentId -> m ()
--rate v Nothing _ _ = errNoSenderId
rate v uid ct cid = runDb $ do
    rating <- getRating uid cid ct
    case rating of
        -- create rating
        Nothing -> (insert $ Rating cid ct uid v) >> return ()
        -- update rating
        (Just e@(Entity k r)) -> update k [RatingValue =. v]

remove :: (MonadIO m, MonadReader Config m) => Id -> ContentType -> ContentId -> m ()
--remove Nothing _ _ = errNoSenderId
remove uid ct cid = runDb $ do
    rating <- getRating uid cid ct
    case rating of
        Nothing -> return ()
        (Just e@(Entity k _)) -> delete k

getRatings :: (MonadIO m, MonadReader Config m) => Id -> ContentType -> ContentId -> m Ratings
--getRatings Nothing _ _ = errNoSenderId
getRatings uid ct cid = runDb $ do
    let filters = ratingFilters cid ct
    likes       <- count        ((RatingValue ==. Like)    : filters) 
    dislikes    <- count        ((RatingValue ==. Dislike) : filters) 
    usersRating <- selectFirst  ((RatingUserId ==. uid)    : filters) []
    let rval   = fmap show $ value usersRating
    return $ Ratings likes dislikes (ContentKey cid ct) uid rval

bulkRatings :: (MonadIO m, MonadReader Config m) => Id -> [ContentKey] -> m [Ratings]
bulkRatings uid cks = mapM (\(ContentKey cid ct) -> getRatings uid ct cid) cks

app :: Config -> Application
app cfg = serve ratingAPI (readerServer cfg)

readerServer :: Config -> Server RatingAPI
readerServer cfg = enter (readerToEither cfg) server
 
readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

apiDocs :: API
apiDocs = docs ratingAPI
updateDocs = writeFile "rating-service.md" $ markdown apiDocs
