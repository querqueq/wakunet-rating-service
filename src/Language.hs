{-# LANGUAGE TemplateHaskell #-}

module Language where

import Database.Persist.TH
--import Data.Aeson          (ToJSON,FromJSON)
import Data.Aeson.TH
import Data.Typeable
import Data.Data

data Language = De | En
    deriving (Show, Read, Eq)
derivePersistField "Language"

$(deriveJSON defaultOptions ''Language)
