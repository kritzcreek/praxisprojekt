{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model1 where

-- import           Control.Lens
-- import           Data.Aeson
-- import           Data.Aeson.TH
-- import           Data.Text       (Text)
-- import qualified Data.Text       as T
-- import           Data.Time
-- import           Data.UUID

-- import Kafkaproto.Util
-- import Kafkaproto.Entity

-- import qualified Test.QuickCheck as QC

-- newtype KundeId = KundeId Int
--                   deriving (Show, Eq, FromJSON, ToJSON)

-- newtype WarenkorbId = WarenkorbId Int
--                   deriving (Show, Eq, FromJSON, ToJSON)

-- newtype BestellungId = BestellungId Int
--                   deriving (Show, Eq, FromJSON, ToJSON)

-- newtype Preis = Preis Integer
--                   deriving (Show, Eq, FromJSON, ToJSON)

-- newtype Adresse = Adresse Text
--                   deriving (Show, Eq, FromJSON, ToJSON)

-- data Kunde =
--   Kunde
--   { _kundeId   :: KundeId
--   , _kundeName :: Text
--   } deriving (Show, Eq)

-- data Warenkorb =
--   Warenkorb
--   { _warenkorbId     :: WarenkorbId
--   , _warenkorbKunde  :: Entity Kunde
--   , _warenkorbPosten :: [WarenkorbPosten]
--   } deriving (Show, Eq)

-- data WarenkorbPosten =
--   WarenkorbPosten
--   { _warenkorbPostenProdukt :: Entity Produkt
--   , _warenkorbPostenAnzahl  :: Int
--   } deriving (Show, Eq)

-- data Bestellung =
--   Bestellung
--   { _bestellungId        :: BestellungId
--   , _bestellungWarenkorb :: Entity Warenkorb
--   , _bestellungPreis     :: Preis
--   , _bestellungAdresse   :: Adresse
--   } deriving (Show, Eq)

-- kunde :: IO (Message Kunde)
-- kunde = QC.generate QC.arbitrary

-- kunde_ :: IO Kunde
-- kunde_ = QC.generate QC.arbitrary

-- warenkorb :: IO (Message Warenkorb)
-- warenkorb = QC.generate QC.arbitrary

-- warenkorb_ :: IO Warenkorb
-- warenkorb_ = QC.generate QC.arbitrary

-- makeLenses ''Kunde
-- makeLenses ''Warenkorb
-- makeLenses ''WarenkorbPosten
-- makeLenses ''Bestellung

-- deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_produkt")})         ''Produkt
-- deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_warenkorb")})       ''Warenkorb
-- deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_warenkorbPosten")}) ''WarenkorbPosten
-- deriveJSON (defaultOptions {fieldLabelModifier=fieldLabelDrop (T.length "_bestellung")})      ''Bestellung

-- -- ghci helper
-- names :: [Text]
-- names =
--   [ "Ernst", "Friedrich", "Hans", "Heinrich", "Hermann", "Karl"
--   , "Otto", "Paul", "Walter", "Wilhelm", "Anna", "Bertha"
--   , "Elisabeth", "Emma", "Frieda ", "Gertrud", "Margarethe"
--   , "Maria", "Marie", "Martha", "Gerhard", "Günter", "Hans"
--   , "Heinz", "Helmut", "Herbert", "Karl", "Kurt", "Walter"
--   , "Werner", "Edith", "Elfriede", "Erna", "Gerda", "Gertrud"
--   , "Hildegard", "Ilse", "Irmgard", "Lieselotte", "Ursula"
--   , "Dieter", "Günter", "Hans", "Horst", "Jürgen", "Klaus"
--   , "Manfred", "Peter", "Uwe", "Wolfgang", "Christa", "Elke"
--   , "Erika", "Gisela", "Helga", "Ingrid", "Karin", "Monika"
--   , "Renate", "Ursula", "Andreas", "Frank", "Jörg", "Jürgen"
--   , "Klaus", "Peter", "Stefan", "Thomas", "Uwe" , "Andrea"
--   , "Angelika", "Birgit", "Gabriele", "Heike", "Martina"
--   , "Petra", "Sabine", "Susanne", "Ute", "Alexander", "Christian"
--   , "Daniel", "Dennis", "Jan", "Martin", "Michael", "Sebastian"
--   , "Stefan", "Thomas", "Anja", "Christina", "Julia", "Katrin"
--   , "Melanie", "Nadine", "Nicole", "Sabrina", "Sandra", "Stefanie"
--   , "Finn", "Jan", "Jannik", "Jonas", "Leon", "Luca", "Lukas"
--   , "Niklas", "Tim", "Tom", "Anna", "Hannah", "Julia", "Lara"
--   , "Laura", "Lea", "Lena", "Lisa", "Michelle", "Sarah"
--   ]

-- kunden :: [Kunde]
-- kunden = zipWith (Kunde . KundeId) [1000..] names

-- instance QC.Arbitrary Kunde where
--   arbitrary = QC.elements kunden

-- instance QC.Arbitrary KundeId where
--   arbitrary = view kundeId <$> QC.arbitrary

-- instance QC.Arbitrary WarenkorbPosten where
--   arbitrary = WarenkorbPosten <$> QC.arbitrary <*> QC.arbitrary `QC.suchThat` (> 0)

-- instance QC.Arbitrary Warenkorb where
--   arbitrary = Warenkorb <$>
--     WarenkorbId <$> QC.arbitrarySizedNatural <*>
--     QC.arbitrary <*>
--     QC.arbitrary

-- instance HasKey Kunde where
--   getKey k =
--     let (KundeId i) = view kundeId k
--     in T.pack (show i)

-- instance HasKey Produkt where
--   getKey p =
--     let (ProduktId i) = view produktId p
--     in T.pack (show i)

-- instance HasKey Warenkorb where
--   getKey w =
--     let (WarenkorbId i) = view warenkorbId w
--     in T.pack (show i)
