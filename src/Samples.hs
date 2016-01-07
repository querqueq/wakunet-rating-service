{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Samples where

import Database.Persist.Sqlite      (insert, selectList, Entity(..)
                                    ,fromSqlKey, (==.), toSqlKey
                                    ,get, PersistEntity,ToBackendKey
                                    ,SqlBackend, getBy)
import Data.Int                     (Int64)
import Data.Time                    (getCurrentTime, fromGregorian, UTCTime(..))

import Models 
import Sex
import Config                       (Environment(..))

import Debug.Trace

populateDb Development = do
    users <- selectList [UserEmail ==. (userEmail $ user 1)] [] 
    -- check if db was already populated
    case users of
         [] -> do
            user1Id <- insert $ user 1
            user2Id <- insert $ user 2
            group1Id <- insert $ group 1 user1Id
            group2Id <- insert $ group 2 user1Id
            content1Id <- insert $ content 1 user2Id group1Id Nothing
            content2Id <- insert $ content 2 user1Id group1Id $ Just content1Id
            insert $ GroupMember group1Id user1Id $ time 2
            insert $ GroupMember group1Id user2Id $ time 3
            insert $ GroupMember group2Id user1Id $ time 3
            return ()
         _  -> return ()
populateDb _ = return ()


type AdminId = UserId
type CreatorId = UserId
type ExampleNo = Integer

dummyText 1 = "Hello everybody!"
dummyText 2 = "Hi, guy"

time 1 = UTCTime (fromGregorian 2015 10 14) (fromIntegral 60*60*12)
time 2 = UTCTime (fromGregorian 2015 10 14) (fromIntegral 60*60*13)
time 3 = UTCTime (fromGregorian 2015 10 14) (fromIntegral 60*60*13+60*30)

user 1 = User "John" "Doe" "john.doe@example.org" "Lorem Ipsum" Male
user 2 = User "Jane" "Doe" "jane.doe@info.com" "Dolor sit amet" Female
users = [user i|i <- [1..2]]

content :: ExampleNo -> CreatorId -> GroupId -> Maybe ContentId -> Content
content 1 = content' (dummyText 1) (time 2)
content 2 = content' (dummyText 2) (time 3) 
contents = [content i (toSqlKey 1) (toSqlKey 1) Nothing|i <- [1..2]]

content' text time cid gid parent = Content cid gid text time parent

group :: ExampleNo -> AdminId -> Group
group 1 = Group "staff"
group 2 = Group "vip"
groups = [group i (toSqlKey 1)|i <- [1..2]]

postWithComments 1 = contentToPostWithComments (Entity parentId $ content 1 (toSqlKey 1) groupId Nothing) $ 
                       map ((flip contentToPostWithComments) []) [Entity (toSqlKey 2) $ content 2 (toSqlKey 2) groupId $ Just parentId]
                    where groupId = toSqlKey 1 :: GroupId
                          parentId = toSqlKey 1 :: ContentId
