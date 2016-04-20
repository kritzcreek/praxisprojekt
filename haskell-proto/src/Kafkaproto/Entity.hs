{-# LANGUAGE DeriveFunctor       #-}
module Kafkaproto.Entity where

import           Data.Aeson
import           Data.Text       (Text)
import qualified Test.QuickCheck as QC

data Entity a = Entity Text (Maybe a)
                deriving (Show, Eq, Functor)

instance ToJSON (Entity a) where
  toJSON (Entity k _) = toJSON k

instance (HasKey a, FromJSON a) => FromJSON (Entity a) where
  parseJSON (String s) = pure (Entity s Nothing)
  parseJSON o = do
    a <- parseJSON o
    return $ Entity (getKey a) (Just a)

instance HasKey (Entity a) where
  getKey (Entity k _) = k

class HasKey a where
  getKey :: a -> Text

instance (QC.Arbitrary a, HasKey a) => QC.Arbitrary (Entity a) where
  arbitrary = do
    a <- QC.arbitrary
    return $ Entity (getKey a) (Just a)

