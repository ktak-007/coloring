{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Application1 {-# DEPRECATED "The first variant" #-} ( run ) where

import Conduit
import Control.Applicative (some)
import Rainbow
import Data.Function ((&))
import Data.ByteString.Char8 (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Text.Parsec
import Text.Parsec.String (Parser)

run :: IO ()
run = runConduit $ stdinC
                .| linesUnboundedAsciiC
                .| mapC (unpack.decodeUtf8)
                .| mapC colorer
                .| mapMC putChunksLn
                .| sinkNull

colorer :: String -> [Chunk]
colorer input = case parse genericParser "" input of
                   Right log -> getColored log
                   _ -> [ p input ]

data Log = StdLog { dateTimePart :: String
                  , splitter1 :: String
                  , levelPart :: String
                  , splitter2 :: String
                  , messageStepPart :: String
                  , splitter3 :: String
                  , messagePart :: String
                  }
         | AtfStep { atfStep :: String
                   , atfSplit1 :: String
                   , atfStatus :: String
                   , atfSplit2 :: String
                   , atfMessage :: String
                   }

genericParser :: Parser Log
genericParser = stdLogParser <|> atfStepsParser

stdLogParser :: Parser Log
stdLogParser = StdLog <$> dateTimeParser
                      <*> some space
                      <*> levelParser
                      <*> some space
                      <*> messageStepParser
                      <*> optionalSplitParser
                      <*> messageParser
  where
  dateTimeParser = timeParser <> string " " <> dateParser
  timeParser = count 2 digit <> string ":" <> count 2 digit <> string ":" <> count 2 digit
  dateParser = count 4 digit <> string "/" <> count 2 digit <> string "/" <> count 2 digit
  levelParser = string "[" <> some upper <> string "]"
  messageStepParser = option "" $ try stepKeywordWithNum

atfStepsParser :: Parser Log
atfStepsParser = AtfStep <$> stepKeywordWithNum
                         <*> splitParser
                         <*> optionalStatusParser
                         <*> optionalSplitParser
                         <*> messageParser
  where optionalStatusParser = option "" $ try $ statusParser <* lookAhead splitParser

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

getColored :: Log -> [Chunk]
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
getColored AtfStep {..} = [ p atfStep & fore (if atfStatus /= "" then blue else red)
                          , p atfSplit1
                          , p atfStatus & fore (if atfStatus == "SUCCESS" then green else red)
                          , p atfSplit2
                          , p atfMessage & fore cyan
                          ]

p :: String -> Chunk
p = chunk.pack
