{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Plesk.Atf ( emptyList ) where

import Control.Applicative (some)
import Data.Function ((&))
import Data.Text (pack)
import Rainbow
import Text.Parsec
import Text.Parsec.String (Parser)

import Log

emptyList :: [Log]
emptyList = [ Log (empty::AtfStdLog)
            , Log (empty::AtfShortLog)
            ]

-- 04:12:38 2022/10/06 [INFO] precondition #1: Create client, subscription and domain (duration: 00.000)
data AtfStdLog = AtfStdLog { dateTime :: String
                           , splitter1 :: String
                           , level :: String
                           , splitter2 :: String
                           , atfStep :: String
                           , splitter3 :: String
                           , message :: String
                           } deriving Show

instance LogLine AtfStdLog where
  empty = AtfStdLog "" "" "" "" "" "" ""
  logLineParser _ = getColored <$> stdLogParser
    where stdLogParser = AtfStdLog <$> dateTimeParser
                                   <*> some space
                                   <*> levelParser
                                   <*> some space
                                   <*> atfStepParser
                                   <*> optionalSplitParser
                                   <*> messageParser
          dateTimeParser = timeParser <> string " " <> dateParser
          timeParser = count 2 digit <> string ":" <> count 2 digit <> string ":" <> count 2 digit
          dateParser = count 4 digit <> string "/" <> count 2 digit <> string "/" <> count 2 digit
          levelParser = string "[" <> some upper <> string "]"
          atfStepParser = option "" $ try stepKeywordWithNum
  getColored AtfStdLog {..} = [ p dateTime & fore grey
                              , p splitter1
                              , p level & fore ( case level of
                                                      "[INFO]" -> blue
                                                      "[WARN]" -> yellow
                                                      "[DEBUG]" -> green
                                                      _ -> red
                                               )
                              , p splitter2
                              , p atfStep & fore yellow & back red
                              , p splitter3
                              , p message & ( case message of
                                                "Run precondition..."  -> fore yellow <&> back blue
                                                "Run steps..." -> fore yellow <&> back blue
                                                "Run verify..." -> fore yellow <&> back blue
                                                _ -> fore cyan
                                            )
                              ]
    where a <&> b = \x -> a $ b x

-- precondition #1: SUCCESS: Hello
data AtfShortLog = AtfShortLog { atfStep :: String
                               , splitter1 :: String
                               , status :: String
                               , splitter2 :: String
                               , message :: String
                               } deriving Show

instance LogLine AtfShortLog where
  empty = AtfShortLog "" "" "" "" ""
  logLineParser _ = getColored <$> atfLogParser
    where atfLogParser = AtfShortLog <$> stepKeywordWithNum
                                     <*> splitParser
                                     <*> optionalStatusParser
                                     <*> optionalSplitParser
                                     <*> messageParser
          optionalStatusParser = option "" $ try $ statusParser <* lookAhead splitParser
  getColored AtfShortLog {..} = [ p atfStep & fore (if status /= "" then blue else red)
                                , p splitter1
                                , p status & fore (if status == "SUCCESS" then green else red)
                                , p splitter2
                                , p message & fore cyan
                                ]

-- Common parsers
splitParser :: Parser String
splitParser = string ":" <> some space

optionalSplitParser :: Parser String
optionalSplitParser = option "" $ try splitParser

messageParser :: Parser String
messageParser = many anyChar

stepKeyword :: Parser String
stepKeyword = choice $ string <$> ["precondition", "steps", "verify"]

stepKeywordWithNum :: Parser String
stepKeywordWithNum = stepKeyword <> string " #" <> some digit

statusParser :: Parser String
statusParser = some upper

p :: String -> Chunk
p = chunk.pack
