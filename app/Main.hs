{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Megaparsec (parse)
import SuperAnki

import AnkiConnect

main :: IO ()
main = do
  addNote (MkClozeNote "General" "This is a {{c1::test}}" []) >>= print
  -- print (parse parseCloze "src" " C: test {test} test")
  -- print (parse parseFile "src" " test\nC\nC: test {test} test\n\na")
