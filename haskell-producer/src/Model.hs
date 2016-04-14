{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time
import           Data.UUID

import qualified Test.QuickCheck as QC
import           Util

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

data Entity a = Entity Text (Maybe a)
                deriving (Show, Eq, Functor)

data Message a =
  Message
  { _messageId      :: Text
  , _messageKey     :: Text
  , _messageTime    :: UTCTime
  , _messageType    :: Text
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
  , _warenkorbKunde  :: Entity Kunde
  , _warenkorbPosten :: [WarenkorbPosten]
  } deriving (Show, Eq)

data WarenkorbPosten =
  WarenkorbPosten
  { _warenkorbPostenProdukt :: Entity Produkt
  , _warenkorbPostenAnzahl  :: Int
  } deriving (Show, Eq)

data Bestellung =
  Bestellung
  { _bestellungId        :: BestellungId
  , _bestellungWarenkorb :: Entity Warenkorb
  , _bestellungPreis     :: Preis
  , _bestellungAdresse   :: Adresse
  } deriving (Show, Eq)

produkt :: IO (Message Produkt)
produkt = QC.generate QC.arbitrary

produkt_ :: IO Produkt
produkt_ = QC.generate QC.arbitrary

kunde :: IO (Message Kunde)
kunde = QC.generate QC.arbitrary

kunde_ :: IO Kunde
kunde_ = QC.generate QC.arbitrary

warenkorb :: IO (Message Warenkorb)
warenkorb = QC.generate QC.arbitrary

warenkorb_ :: IO Warenkorb
warenkorb_ = QC.generate QC.arbitrary

makeLenses ''Message
makeLenses ''Kunde
makeLenses ''Produkt
makeLenses ''Warenkorb
makeLenses ''WarenkorbPosten
makeLenses ''Bestellung

deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_message")})         ''Message
deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_kunde")})           ''Kunde
deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_produkt")})         ''Produkt
deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_warenkorb")})       ''Warenkorb
deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_warenkorbPosten")}) ''WarenkorbPosten
deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_bestellung")})      ''Bestellung

instance ToJSON (Entity a) where
  toJSON (Entity k _) = toJSON k

instance (HasKey a, FromJSON a) => FromJSON (Entity a) where
  parseJSON (String s) = pure (Entity s Nothing)
  parseJSON o = do
    a <- parseJSON o
    return $ Entity (getKey a) (Just a)

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
kunden = zipWith (Kunde . KundeId) [1000..] names

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
  zipWith mkProdukt [100..] produktDaten
  where
    mkProdukt i (n, p) = Produkt (ProduktId i) n (Preis p)

instance QC.Arbitrary Kunde where
  arbitrary = QC.elements kunden

instance QC.Arbitrary KundeId where
  arbitrary = view kundeId <$> QC.arbitrary

instance QC.Arbitrary Produkt where
  arbitrary = QC.elements produkte

instance QC.Arbitrary ProduktId where
  arbitrary = view produktId <$> QC.arbitrary

instance QC.Arbitrary WarenkorbPosten where
  arbitrary = WarenkorbPosten <$> QC.arbitrary <*> QC.arbitrary `QC.suchThat` (> 0)

instance QC.Arbitrary Warenkorb where
  arbitrary = Warenkorb <$>
    WarenkorbId <$> QC.arbitrarySizedNatural <*>
    QC.arbitrary <*>
    QC.arbitrary

instance (QC.Arbitrary a, HasKey a) => QC.Arbitrary (Entity a) where
  arbitrary = do
    a <- QC.arbitrary
    return $ Entity (getKey a) (Just a)

instance (QC.Arbitrary a, HasKey a) => QC.Arbitrary (Message a) where
  arbitrary = do
    ArbUUID mid <- QC.arbitrary
    payload <- QC.arbitrary
    ArbUTCTime time <- QC.arbitrary
    mtype <- QC.elements ["create", "update", "delete"]
    return $ Message (toText mid) (getKey payload) time mtype payload

class HasKey a where
  getKey :: a -> Text

instance HasKey Kunde where
  getKey k =
    let (KundeId i) = view kundeId k
    in T.pack (show i)

instance HasKey Produkt where
  getKey p =
    let (ProduktId i) = view produktId p
    in T.pack (show i)

instance HasKey Warenkorb where
  getKey w =
    let (WarenkorbId i) = view warenkorbId w
    in T.pack (show i)

instance HasKey (Entity a) where
  getKey (Entity k _) = k

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
    return $ ArbUTCTime $ UTCTime (fromGregorian randomYear randomMonth randomDay) (fromIntegral randomTime)
