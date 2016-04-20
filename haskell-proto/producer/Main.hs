module Main where

import Control.Monad
import Control.Concurrent (threadDelay, forkIO)
import Haskakafka
import qualified Data.ByteString.Char8 as C8
import System.Exit (exitSuccess)

kafkaConfig :: [(String, String)]
kafkaConfig = [("socket.timeout.ms", "50000")]

topicConfig :: [(String, String)]
topicConfig = [("request.timeout.ms", "50000")]

connectionString :: String
connectionString = "192.168.99.100:9092"

partition :: Int
partition = 0

main :: IO ()
main = do
  -- Consumer der alle messages auf stdout ausgibt
  _ <- forkIO (loggingConsumer "topic")
  -- Nimmt solange Messages an, bis "quit" eingegeben wird
  withKafkaProducer kafkaConfig topicConfig connectionString "topic" $
    \_ topic -> handleInput topic
  -- Bricht den log thread ab
  exitSuccess

handleInput :: KafkaTopic -> IO ()
handleInput topic = do
  input <- C8.getLine
  if C8.unpack input == "quit"
    then pure ()
    else produceMessage topic (KafkaSpecifiedPartition partition) (KafkaProduceMessage input)
         >> handleInput topic

loggingConsumer :: String -> IO ()
loggingConsumer topicString =
  withKafkaConsumer kafkaConfig topicConfig
                    connectionString topicString
                    partition -- locked to a specific partition for each consumer
                    KafkaOffsetEnd -- start reading from beginning (alternatively, use
                                        -- KafkaOffsetEnd, KafkaOffset or KafkaOffsetStored)
                    $ \kafka topic -> do
    setLogLevel kafka KafkaLogCrit
    forever (threadDelay 1000000 >> consumeSingle topic)
  where
  consumeSingle :: KafkaTopic -> IO ()
  consumeSingle topic =
    consumeMessage topic partition 1000 >>= either
      (const (pure ()))
      (C8.putStrLn . (C8.pack "Message: " `C8.append`) . messagePayload)