module Main where

import           Control.Concurrent    (forkIO, threadDelay)
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Haskakafka
import           Model
import           System.Exit           (exitSuccess)
import qualified Kafkaproto.Message as KP

kafkaConfig :: [(String, String)]
kafkaConfig = [("socket.timeout.ms", "50000")]

topicConfig :: [(String, String)]
topicConfig = [("request.timeout.ms", "50000")]

connectionString :: String
connectionString = "192.168.99.100:9092"

partition :: Int
partition = 0

loggingConsumer :: String -> IO ()
loggingConsumer topicString =
  withKafkaConsumer kafkaConfig topicConfig
                    connectionString topicString
                    partition -- locked to a specific partition for each consumer
                    KafkaOffsetEnd -- start reading from beginning (alternatively, use
                                        -- KafkaOffsetEnd, KafkaOffset or KafkaOffsetStored)
                    $ \kafka topic -> do
    setLogLevel kafka KafkaLogCrit
    forever $ consumeSingle topic
  where
  consumeSingle :: KafkaTopic -> IO ()
  consumeSingle topic =
    consumeMessage topic partition 1000 >>= either
      (const (threadDelay 1000000))
      (print . fmap (view (produktName)) . decodeProduct)

decodeProduct :: KafkaMessage -> Maybe Produkt
decodeProduct m =
  let payload = messagePayload m
      produkt = decodeStrict payload :: Maybe (KP.Message Produkt)
  in view KP.messagePayload <$> produkt

main :: IO ()
main = loggingConsumer "produkte"
