{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import ClassyPrelude hiding (getArgs)
import Nix
import System.Environment (getArgs)
import Text.Render


main :: IO ()
main = getArgs >>= \case
  "--pretty":file:_ -> parseFile file >>= \case
    Left err -> error $ show err
    Right e -> putStrLn $ renderIndented 2 e
  file:_ -> parseFile file >>= \case
    Left err -> error $ unlines ["When parsing file " <> file, show err]
    Right e -> print e
  _ -> error "Please supply a file as an argument"
