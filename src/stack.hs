#!/usr/bin/env stack
{- stack
   script
   --resolver lts-19.25
   --package text
   --package rainbow
   --package conduit
   --package bytestring
   --package parsec
-}

module Main (main) where

import Application

main :: IO ()
main = run
