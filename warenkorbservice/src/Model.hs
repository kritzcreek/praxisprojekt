{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model where

import           Control.Lens
import           Data.Aeson
import           Data.Text          (Text)

newtype ProduktId = ProduktId Int
  deriving (Show, Eq, FromJSON, ToJSON)

newtype Preis = Preis Integer
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Enum, Real, Num, Integral)

data Produkt =
  Produkt
  { _produktId           :: ProduktId
  , _produktName         :: Text
  , _produktPreis        :: Preis
  } deriving (Show, Eq)

makeLenses ''Produkt

instance FromJSON Produkt where
  parseJSON = withObject "Produkt" $ \o ->
    Produkt
    <$> o .: "id"
    <*> o .: "name"
    <*> (berechnePreis <$> o .: "preis" <*> o.: "rabatt")
    where
      berechnePreis :: (Integral a) => a -> Double -> a
      berechnePreis preis rabatt = floor $ fromIntegral preis * (1 - rabatt / 100)
