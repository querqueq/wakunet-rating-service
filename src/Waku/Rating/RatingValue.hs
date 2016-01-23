{-# LANGUAGE TemplateHaskell #-}

module Waku.Rating.RatingValue where

import Database.Persist.TH
import Data.Aeson.TH

data RatingValue = Like | Dislike
    deriving (Show, Read, Eq)
derivePersistField "RatingValue"

$(deriveJSON defaultOptions ''RatingValue)
