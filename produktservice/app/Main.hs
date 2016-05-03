module Main where

import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Haskakafka
import           Model
import           System.Exit          (exitSuccess)

kafkaConfig :: [(String, String)]
kafkaConfig = [("socket.timeout.ms", "50000")]

topicConfig :: [(String, String)]
topicConfig = [("request.timeout.ms", "50000")]

connectionString :: String
connectionString = "172.17.0.1:9092"

partition :: Int
partition = 0

main :: IO ()
main = do
  withKafkaProducer kafkaConfig topicConfig connectionString "produkte" $
    \_ topic -> replicateM_ 100 $ do
      void $ produceMessage topic (KafkaSpecifiedPartition partition)
        . KafkaProduceMessage
        . BSL.toStrict
        . encode =<< produkt
      threadDelay 100000
