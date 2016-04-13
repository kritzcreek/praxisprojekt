{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Model where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text     (Text)
import           Data.Time
import           Data.UUID
import           Data.UUID.V4

newtype KundeId = KundeId Int
                  deriving (Show, Eq, FromJSON, ToJSON)

newtype ProduktId = ProduktId Int
                  deriving (Show, Eq, FromJSON, ToJSON)

newtype WarenkorbId = WarenkorbId Int
                  deriving (Show, Eq, FromJSON, ToJSON)

newtype BestellungId = BestellungId Int
                  deriving (Show, Eq, FromJSON, ToJSON)

newtype Preis = Preis Integer
                  deriving (Show, Eq, FromJSON, ToJSON)

newtype Adresse = Adresse Text
                  deriving (Show, Eq, FromJSON, ToJSON)

data Message a =
  Message
  { _messageId      :: Text
  , _messageKey     :: Text
  , _messageTime    :: UTCTime
  , _messageType    :: String
  , _messagePayload :: a
  } deriving (Show, Eq, Functor)

data Kunde =
  Kunde
  { _kundeId   :: KundeId
  , _kundeName :: Text
  } deriving (Show, Eq)

data Produkt =
  Produkt
  { _produktId    :: ProduktId
  , _produktName  :: Text
  , _produktPreis :: Preis
  } deriving (Show, Eq)

data Warenkorb =
  Warenkorb
  { _warenkorbId     :: WarenkorbId
  , _warenkorbKunde  :: KundeId
  , _warenkorbPosten :: [WarenkorbPosten]
  } deriving (Show, Eq)

data WarenkorbPosten =
  WarenkorbPosten
  { _warenkorbPostenProdukt :: ProduktId
  , _warenkorbPostenAnzahl  :: Int
  } deriving (Show, Eq)

data Bestellung =
  Bestellung
  { _bestellungId        :: BestellungId
  , _bestellungWarenkorb :: WarenkorbId
  , _bestellungPreis     :: Preis
  , _bestellungAdresse   :: Adresse
  } deriving (Show, Eq)

makeLenses ''Message
makeLenses ''Kunde
makeLenses ''Produkt
makeLenses ''Warenkorb
makeLenses ''WarenkorbPosten
makeLenses ''Bestellung

deriveJSON defaultOptions ''Message
deriveJSON defaultOptions ''Kunde
deriveJSON defaultOptions ''Produkt
deriveJSON defaultOptions ''Warenkorb
deriveJSON defaultOptions ''WarenkorbPosten
deriveJSON defaultOptions ''Bestellung

-- ghci helper
names :: [Text]
names =
  [ "Ernst", "Friedrich", "Hans", "Heinrich", "Hermann", "Karl"
  , "Otto", "Paul", "Walter", "Wilhelm", "Anna", "Bertha"
  , "Elisabeth", "Emma", "Frieda ", "Gertrud", "Margarethe"
  , "Maria", "Marie", "Martha", "Gerhard", "Günter", "Hans"
  , "Heinz", "Helmut", "Herbert", "Karl", "Kurt", "Walter"
  , "Werner", "Edith", "Elfriede", "Erna", "Gerda", "Gertrud"
  , "Hildegard", "Ilse", "Irmgard", "Lieselotte", "Ursula"
  , "Dieter", "Günter", "Hans", "Horst", "Jürgen", "Klaus"
  , "Manfred", "Peter", "Uwe", "Wolfgang", "Christa", "Elke"
  , "Erika", "Gisela", "Helga", "Ingrid", "Karin", "Monika"
  , "Renate", "Ursula", "Andreas", "Frank", "Jörg", "Jürgen"
  , "Klaus", "Peter", "Stefan", "Thomas", "Uwe" , "Andrea"
  , "Angelika", "Birgit", "Gabriele", "Heike", "Martina"
  , "Petra", "Sabine", "Susanne", "Ute", "Alexander", "Christian"
  , "Daniel", "Dennis", "Jan", "Martin", "Michael", "Sebastian"
  , "Stefan", "Thomas", "Anja", "Christina", "Julia", "Katrin"
  , "Melanie", "Nadine", "Nicole", "Sabrina", "Sandra", "Stefanie"
  , "Finn", "Jan", "Jannik", "Jonas", "Leon", "Luca", "Lukas"
  , "Niklas", "Tim", "Tom", "Anna", "Hannah", "Julia", "Lara"
  , "Laura", "Lea", "Lena", "Lisa", "Michelle", "Sarah"
  ]

kunden :: [Kunde]
kunden =
  map mkKunde (zip [1..] names)
  where
    mkKunde (i, n) = Kunde (KundeId i) n

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

produkte :: [Produkt]
produkte =
  map mkProdukt (zip [1..] produktDaten)
  where
    mkProdukt (i, (n, p)) = Produkt (ProduktId i) n (Preis p)

uuids :: IO [Text]
uuids = mapM (const (fmap toText nextRandom)) ([0..100] :: [Int])
