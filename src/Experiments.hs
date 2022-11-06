{-# LANGUAGE OverloadedStrings, RecordWildCards, ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

{-
  Module to check ideas
  You can load this module with the commands:
  ```
  stack exec ghci
  :l "src/Experiments.hs"
  ```
  and after that you can play with this module and check your ideas.
  Try the command `go` for example.
-}
module Experiments where

import Conduit
import Control.Applicative (some)
import Rainbow
import Data.Function ((&))
import Data.ByteString.Char8 (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor

class Show a => LogLine a where
  empty :: a
  logLineParser :: a -> Parser [Chunk]
  getColored :: a -> [Chunk]

data StdLog = StdLog { dateTimePart :: String
                     , splitter1 :: String
                     , levelPart :: String
                     , splitter2 :: String
                     , messageStepPart :: String
                     , splitter3 :: String
                     , messagePart :: String
                     } deriving Show

instance LogLine StdLog where
  empty = StdLog "" "" "" "" "" "" ""
  logLineParser _ = getColored <$> stdLogParser
    where stdLogParser = StdLog <$> dateTimeParser
                                <*> some space
                                <*> levelParser
                                <*> some space
                                <*> messageStepParser
                                <*> optionalSplitParser
                                <*> messageParser
          dateTimeParser = timeParser <> string " " <> dateParser
          timeParser = count 2 digit <> string ":" <> count 2 digit <> string ":" <> count 2 digit
          dateParser = count 4 digit <> string "/" <> count 2 digit <> string "/" <> count 2 digit
          levelParser = string "[" <> some upper <> string "]"
          messageStepParser = option "" $ try stepKeywordWithNum
  getColored StdLog {..} = [ p dateTimePart & fore grey
                           , p splitter1
                           , p levelPart & fore ( case levelPart of
                                                    "[INFO]" -> blue
                                                    "[WARN]" -> yellow
                                                    "[DEBUG]" -> green
                                                    _ -> red
                                                )
                           , p splitter2
                           , p messageStepPart & fore yellow & back red
                           , p splitter3
                           , p messagePart & fore cyan
                           ]

data AtfStep = AtfStep { atfStep :: String
                       , atfSplit1 :: String
                       , atfStatus :: String
                       , atfSplit2 :: String
                       , atfMessage :: String
                       } deriving Show
instance LogLine AtfStep where
  empty = AtfStep "" "" "" "" ""
  logLineParser _ = getColored <$> atfLogParser
    where atfLogParser = AtfStep <$> stepKeywordWithNum
                                 <*> splitParser
                                 <*> optionalStatusParser
                                 <*> optionalSplitParser
                                 <*> messageParser
          optionalStatusParser = option "" $ try $ statusParser <* lookAhead splitParser
  getColored AtfStep {..} = [ p atfStep & fore (if atfStatus /= "" then blue else red)
                            , p atfSplit1
                            , p atfStatus & fore (if atfStatus == "SUCCESS" then green else red)
                            , p atfSplit2
                            , p atfMessage & fore cyan
                            ]

data Log = forall a. LogLine a => Log a
instance Show Log where
 show (Log a) = "Log " <> show a

logList :: [Log]
logList = [ Log (empty::StdLog)
          , Log (empty::AtfStep)
          ]

parsersList :: [Parser [Chunk]]
parsersList = map f logList
  where f log@(Log logtype) = logLineParser logtype

example = ["12:01:02 2022/10/24 [INFO] Hello", "steps #1: Hello", "Something else"]

parsedExample = map (\x -> parse (choice parsersList) x x) example

output = map f parsedExample where f = \case Right r -> r
                                             Left parseError -> [p $ sourceName $ errorPos parseError]
go = mapM_ putChunksLn output

stepKeyword :: Parser String
stepKeyword = choice $ string <$> ["precondition", "steps", "verify"]

stepKeywordWithNum :: Parser String
stepKeywordWithNum = stepKeyword <> string " #" <> some digit

messageParser :: Parser String
messageParser = many anyChar

statusParser :: Parser String
statusParser = some upper

splitParser :: Parser String
splitParser = string ":" <> some space

optionalSplitParser :: Parser String
optionalSplitParser = option "" $ try splitParser

p :: String -> Chunk
p = chunk.pack
