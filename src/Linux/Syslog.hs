{-# LANGUAGE RecordWildCards #-}

module Linux.Syslog ( parser ) where

import Control.Applicative (some)
import Data.Function ((&))
import Data.Text (pack)
import Rainbow
import Text.Parsec
import Text.Parsec.String (Parser)

parser :: Parser [Chunk]
parser = sysLogParser

-- Nov  6 22:31:30 ssmirnov-ThinkPad-T480 org.kde.ActivityManager[2708]: After the adjustment
data SysLog = SysLog { dateTime :: String
                     , computer :: String
                     , service :: String
                     , message :: String
                     }

sysLogParser :: Parser [Chunk]
sysLogParser = getColored <$> slParser
  where
  slParser = SysLog <$> dateTimeParser
                    <*> computerParser
                    <*> serviceParser
                    <*> messageParser
  dateTimeParser = dateParser <> string " " <> timeParser
  dateParser = monthParser <> some space <> some digit
  monthParser = choice $ string <$> ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
  timeParser = count 2 digit <> string ":" <> count 2 digit <> string ":" <> count 2 digit
  computerParser = string " " *> some (noneOf " ")
  serviceParser = string " " *> some (noneOf " :")
  messageParser = many anyChar

  getColored SysLog {..} = [ p dateTime
                           , p " "
                           , p computer & fore grey
                           , p " "
                           , p service & fore blue
                           , p message
                           ]
  p = chunk . pack
