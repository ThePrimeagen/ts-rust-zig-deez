{-# LANGUAGE ImportQualifiedPost #-}

module Spec where

import LexerSpec qualified as LexerSpec (spec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  LexerSpec.spec
