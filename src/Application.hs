{-# LANGUAGE LambdaCase #-}

module Application ( run ) where

import Conduit
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Rainbow
import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Plesk.Atf as Atf
import qualified Plesk.Panel as Panel
import qualified Linux.Syslog as SL

parsersList :: [Parser [Chunk]]
parsersList = [ Atf.parser, Panel.parser, SL.parser ]

genericParser :: Parser [Chunk]
genericParser = choice parsersList

run :: IO ()
run = runConduit $ stdinC
                .| linesUnboundedAsciiC
                .| mapC (unpack.decodeUtf8)
                .| mapC parseLine
                .| mapC chunkUnparsed
                .| mapMC putChunksLn
                .| sinkNull

parseLine :: String -> Either ParseError [Chunk]
parseLine line = parse genericParser line line

chunkUnparsed :: Either ParseError [Chunk] -> [Chunk]
chunkUnparsed = \case Right parsed -> parsed
                      Left parseError -> [chunk.pack $ sourceName $ errorPos parseError]
