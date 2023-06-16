{-# LANGUAGE RecordWildCards #-}

module Main where

import Options

main :: IO ()
main = do
    Options{..} <- getOptions
    interact $ show . parser . lexer
