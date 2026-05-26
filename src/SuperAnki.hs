{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SuperAnki where

import Control.Monad.Combinators

import Text.Megaparsec (Parsec, noneOf, try)
import Text.Megaparsec.Char

import Data.Text (Text, pack)
import Data.Void
import Data.Maybe
import Data.Char (ord)
import Data.Functor

import Numeric.Natural (Natural)

import qualified Control.Lens as L
import GHC.Unicode (isDigit, isAsciiLower, isAsciiUpper)
import qualified Data.Map as M

type Parser = Parsec Void Text

base62 :: L.Prism' String Natural
base62 = L.prism to from
  where
    alphabet = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] -- Data.Array

    to :: Natural -> String
    to 0 = "0"
    to x = reverse $ go x
      where
        go :: Natural -> String
        go 0 = ""
        go n =
          let (a, b) = n `divMod` 62 in
          (alphabet !! fromIntegral b) : go a -- TODO: assert 62 = length alphabet

    from :: String -> Either String Natural
    from s = foldl (\acc x -> 62 * acc + x) 0 <$> traverse charVal s 
      where
        charVal c
          | isDigit c = Right $ fromIntegral (ord c - ord '0')
          | isAsciiLower c = Right $ fromIntegral (ord c - ord 'a') + 10
          | isAsciiUpper c = Right $ fromIntegral (ord c - ord 'A') + 36
          | otherwise = Left "wrong char"

data ClozeText = Deletion Text
               | Text Text
               deriving (Show, Eq)

data Cloze = MkCloze { id :: Maybe ClozeID
                     , text :: [ClozeText]
                     }
                     deriving (Show, Eq)

parseDeletion :: Parser ClozeText
parseDeletion = Deletion . pack <$> between (char '{') (char '}') (some (noneOf ['{', '}', '\n']))

parseText :: Parser ClozeText
parseText = Text . pack <$> some (noneOf ['{', '}', '\n'])

parseBase62 :: Parser String
parseBase62 =
  some (lowerChar <|> upperChar <|> digitChar)

parseCloze :: Parser Cloze
parseCloze = do
  hspace
  _ <- char 'C'
  cid <- optional parseBase62
  _ <- char ':'
  MkCloze (cid >>= L.preview base62 <&> ClozeID) <$> many (parseText <|> parseDeletion)

parseLine :: Parser (Maybe Cloze)
parseLine = (Just <$> try parseCloze) <|> (Nothing <$ skipMany (noneOf ['\n']))

parseFile :: Parser [Cloze]
parseFile = catMaybes <$> sepEndBy parseLine newline

newtype AnkiID = AnkiID Natural
                 deriving (Show, Eq)

newtype ClozeID = ClozeID Natural
                  deriving (Show, Eq)

data State = MkState { clozes :: M.Map ClozeID [Cloze]
                     , ankiMap :: M.Map Natural AnkiID
                     }

processNote :: State -> Cloze -> State
processNote state (MkCloze id text) = 
  undefined
  -- case id of
  --   Nothing ->
      
