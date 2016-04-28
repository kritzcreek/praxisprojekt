{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Test.QuickCheck    as QC

import           Common.Entity
import           Common.Message
import           Common.Util

newtype ProduktId = ProduktId Int
  deriving (Show, Eq, FromJSON, ToJSON)

newtype Preis = Preis Integer
  deriving (Show, Eq, FromJSON, ToJSON)

data Produkt =
  Produkt
  { _produktId           :: ProduktId
  , _produktName         :: Text
  , _produktBeschreibung :: Text
  , _produktPreis        :: Preis
  , _produktRabatt       :: Int
  } deriving (Show, Eq)

makeLenses ''Produkt
deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_produkt")}) ''Produkt

produktDaten :: [(Text, Integer)]
produktDaten =
  [ ("Artischocken", 150) , ("Auberginen", 150) , ("Bambussprossen", 30)
  , ("Blumenkohl", 150) , ("Brokkoli", 150) , ("Chicorée", 5)
  , ("Chinakohl", 5) , ("Endivie", 3) , ("Feldsalat", 3)
  , ("Fenchel", 150) , ("Grünkohl", 150) , ("Gewürzgurken", 5)
  , ("Karotten", 150) , ("Kohlrabi", 100) , ("Kopfsalat", 3)
  , ("Kresse", 2) , ("Kürbis", 150) , ("Lauch", 150) , ("Mais", 100)
  , ("Oliven", 3) , ("Paprika", 100) , ("Radieschen", 5) , ("Rosenkohl", 150)
  , ("Rote Bete", 150) , ("Rotkraut", 150) , ("Salatgurken", 150)
  , ("Sauerampfer", 2) , ("Sauerkraut", 150) , ("Schnittlauch", 2)
  , ("Schwarzwurzeln", 150) , ("Sellerie", 5) , ("Spargel", 200)
  , ("Spinat", 200) , ("Tapioka", 1) , ("Tomaten", 150) , ("Weißkraut", 150)
  , ("Wirsing", 150) , ("Zucchini", 150) , ("Zwiebeln", 2)
  ]

-- ProduktIds gehen von 100 - 139
produkte :: [Produkt]
produkte =
  zipWith mkProdukt [100..] produktDaten
  where
    mkProdukt i (n, p) = Produkt (ProduktId i) n "" (Preis p) 0

produkt :: IO (Message Produkt)
produkt = QC.generate QC.arbitrary

produkt_ :: IO Produkt
produkt_ = QC.generate QC.arbitrary

instance QC.Arbitrary Produkt where
  arbitrary = do
    p <- QC.elements produkte
    b <- QC.arbitrary
    r <- QC.choose (0, 50)
    pure (p & produktBeschreibung .~ T.pack b & produktRabatt .~ r)

instance QC.Arbitrary ProduktId where
  arbitrary = view produktId <$> QC.arbitrary

instance HasKey Produkt where
  getKey p =
    let (ProduktId i) = view produktId p
    in T.pack (show i)
