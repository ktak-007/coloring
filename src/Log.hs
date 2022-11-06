{-# LANGUAGE ExistentialQuantification #-}

module Log ( Log(..)
           , LogLine(..)
           ) where

import Rainbow
import Text.Parsec.String (Parser)

class Show a => LogLine a where
  empty :: a
  logLineParser :: a -> Parser [Chunk]
  getColored :: a -> [Chunk]

data Log = forall a. LogLine a => Log a

instance Show Log where
 show (Log a) = "Log " <> show a
