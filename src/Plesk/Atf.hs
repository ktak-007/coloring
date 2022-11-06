{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Plesk.Atf ( parser ) where

import Control.Applicative (some)
import Data.Function ((&))
import Data.Text (pack)
import Rainbow
import Text.Parsec
import Text.Parsec.String (Parser)

parser :: Parser [Chunk]
parser = atfStdParser <|> atfShortParser

-- 04:12:38 2022/10/06 [INFO] precondition #1: Create client, subscription and domain (duration: 00.000)
data AtfStdLog = AtfStdLog { dateTime :: String
                           , splitter1 :: String
                           , level :: String
                           , splitter2 :: String
                           , message :: AtfStdMessage
                           } deriving Show
data AtfStdMessage = AtfStdMessageNormal String
                   | AtfStdMessageRun String
                   | AtfStdStep String String
                   deriving Show

atfStdParser :: Parser [Chunk]
atfStdParser = getColored <$> stdLogParser
    where stdLogParser = AtfStdLog <$> dateTimeParser
                                   <*> some space
                                   <*> levelParser
                                   <*> some space
                                   <*> messageParser
          dateTimeParser = timeParser <> string " " <> dateParser
          timeParser = count 2 digit <> string ":" <> count 2 digit <> string ":" <> count 2 digit
          dateParser = count 4 digit <> string "/" <> count 2 digit <> string "/" <> count 2 digit
          levelParser = string "[" <> some upper <> string "]"
          messageParser = runnerMessage <|> stepMessage <|> normalMessage
          runnerMessage = AtfStdMessageRun <$> try (string "Run " <> stepKeyword <> string "...")
          stepMessage = AtfStdStep <$> try stepKeywordWithNum <*> many anyChar
          normalMessage = AtfStdMessageNormal <$> many anyChar

          getColored AtfStdLog {..} = [ p dateTime & fore grey
                                      , p splitter1
                                      , p level & fore ( case level of
                                                              "[INFO]" -> blue
                                                              "[WARN]" -> yellow
                                                              "[DEBUG]" -> green
                                                              _ -> red
                                                       )
                                      , p splitter2
                                      ]
                                      <>
                                      case message of
                                           AtfStdMessageRun msg -> [ p msg & fore yellow & back blue ]
                                           AtfStdStep step msg -> [ p step & fore yellow & back red, p msg & fore cyan ]
                                           AtfStdMessageNormal msg -> [ p msg & fore cyan ]

-- precondition #1: SUCCESS: Hello
data AtfShortLog = AtfShortLog { atfStep :: String
                               , splitter1 :: String
                               , status :: String
                               , splitter2 :: String
                               , message :: String
                               } deriving Show

atfShortParser :: Parser [Chunk]
atfShortParser = getColored <$> atfLogParser
    where atfLogParser = AtfShortLog <$> stepKeywordWithNum
                                     <*> splitParser
                                     <*> optionalStatusParser
                                     <*> optionalSplitParser
                                     <*> messageParser
          splitParser = string ":" <> some space
          optionalStatusParser = option "" $ try $ some upper <* lookAhead splitParser
          optionalSplitParser = option "" $ try splitParser
          messageParser = many anyChar

          getColored AtfShortLog {..} = [ p atfStep & fore (if status /= "" then blue else red)
                                        , p splitter1
                                        , p status & fore (if status == "SUCCESS" then green else red)
                                        , p splitter2
                                        , p message & fore cyan
                                        ]

-- Common parsers
stepKeyword :: Parser String
stepKeyword = choice $ string <$> ["precondition", "steps", "verify"]

stepKeywordWithNum :: Parser String
stepKeywordWithNum = stepKeyword <> string " #" <> some digit

p :: String -> Chunk
p = chunk.pack
