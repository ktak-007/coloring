{-# LANGUAGE RecordWildCards #-}

module Plesk.Panel ( parser ) where

import Control.Applicative (some)
import Data.Function ((&))
import Data.Text (pack)
import Rainbow
import Text.Parsec
import Text.Parsec.String (Parser)

parser :: Parser [Chunk]
parser = panelLogParser

-- [2022-11-06 03:49:01.725] 1341499:6366cc3db0e2c INFO [performance][] cli /usr/local/psa/admin/plib/modules/monitoring/scripts/cloud-alerts.php : Memory used: 10.00 MB

data PanelLog = PanelLog { dateTime :: String
                         , logId :: String
                         , level :: String
                         , extension :: String
                         , message :: String
                         }

panelLogParser :: Parser [Chunk]
panelLogParser = getColored <$> logParser
  where logParser = PanelLog <$> dateTimeParser
                             <*> idParser
                             <*> levelParser
                             <*> extensionParser
                             <*> messageParser
        dateTimeParser = string "[" <> dateParser <> string " " <> timeParser <> string "]"
        dateParser = count 4 digit <> string "-" <> count 2 digit <> string "-" <> count 2 digit
        timeParser = count 2 digit <> string ":" <> count 2 digit <> string ":" <> count 2 digit <> string "." <> count 3 digit
        idParser = string " " *> some digit <> string ":" <> some hexDigit
        levelParser = string " " *> (   string "INFO"
                                    <|> string "DEBUG"
                                    <|> string "WARN"
                                    <|> string "ERR"
                                    )
        extensionParser = string " " *> string "[" <> some (letter <|> oneOf "/_-") <> string "][]"
        messageParser = string " " *> many anyChar

        getColored PanelLog {..} = [ p dateTime & fore grey
                                   , p " "
                                   , p logId & fore grey
                                   , p " "
                                   , p level & fore ( case level of
                                                           "INFO" -> blue
                                                           "DEBUG" -> green
                                                           "WARN" -> yellow
                                                           _ -> red
                                                    )
                                   , p " "
                                   , p extension
                                   , p " "
                                   , p message & fore ( case level of
                                                             "ERR" -> red
                                                             _ -> cyan
                                                      )
                                   ]
        p = chunk . pack
