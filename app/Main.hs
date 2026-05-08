{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad.Combinators

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text
import Data.Void
import Data.Maybe

type Parser = Parsec Void Text

data ClozeText = Deletion Text
               | Text Text
               deriving (Show)

data Cloze = MkCloze { id :: Maybe Int
                     , text :: [ClozeText]
                     }
                     deriving (Show)

parseDeletion :: Parser ClozeText
parseDeletion = Deletion . pack <$> between (char '{') (char '}') (some (noneOf ['{', '}', '\n']))

parseText :: Parser ClozeText
parseText = Text . pack <$> some (noneOf ['{', '}', '\n'])

parseCloze :: Parser Cloze
parseCloze = hspace *> string "C:" *> hspace *> (MkCloze Nothing <$> many (parseText <|> parseDeletion))

parseLine :: Parser (Maybe Cloze)
parseLine = (Just <$> try parseCloze) <|> (Nothing <$ skipMany (noneOf ['\n']))

parseFile :: Parser [Cloze]
parseFile = catMaybes <$> sepEndBy parseLine newline

main :: IO ()
main = do
  print (parse parseCloze "src" " C: test {test} test")
  print (parse parseFile "src" " test\nC\nC: test {test} test\n\na")
