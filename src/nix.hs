#!/usr/bin/env nix-shell
#!nix-shell --pure --keep BIN_DIR -i "runghc -i${BIN_DIR}"
#!nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ conduit rainbow ])"

module Main (main) where

import Application

main :: IO ()
main = run
