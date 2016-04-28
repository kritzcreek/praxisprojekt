{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Common.Message where

import           Common.Entity
import           Common.Util
import           Control.Lens
import           Data.Aeson.TH
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time
import           Data.UUID
import qualified Test.QuickCheck as QC

data Message a =
  Message
  { _messageId      :: Text
  , _messageKey     :: Text
  , _messageTime    :: UTCTime
  , _messageType    :: Text
  , _messagePayload :: a
  } deriving (Show, Eq, Functor)

makeLenses ''Message
deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_message")}) ''Message

instance (QC.Arbitrary a, HasKey a) => QC.Arbitrary (Message a) where
  arbitrary = do
    ArbUUID mid <- QC.arbitrary
    payload <- QC.arbitrary
    ArbUTCTime time <- QC.arbitrary
    mtype <- QC.elements ["create", "update", "delete"]
    return $ Message (toText mid) (getKey payload) time mtype payload

-- newtype wrapping

newtype ArbUUID = ArbUUID UUID
instance QC.Arbitrary ArbUUID where
  arbitrary = ArbUUID <$> QC.choose (nil, nil)

newtype ArbUTCTime = ArbUTCTime UTCTime
instance QC.Arbitrary ArbUTCTime where
  arbitrary = do
    randomDay <- QC.choose (1, 29) :: QC.Gen Int
    randomMonth <- QC.choose (1, 12) :: QC.Gen Int
    randomYear <- QC.choose (2001, 2002) :: QC.Gen Integer
    randomTime <- QC.choose (0, 86401) :: QC.Gen Int
    pure $ ArbUTCTime $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)
