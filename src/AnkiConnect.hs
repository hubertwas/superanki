{-# LANGUAGE OverloadedStrings #-}

module AnkiConnect where

import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

import System.Process (spawnProcess)

import qualified Data.ByteString.Lazy as LBS
import Control.Monad (void)

data ClozeNote = MkClozeNote
  { deckName  :: Text
  , text     :: Text
  , tags      :: [Text]
  }

data AnkiResponse = MkAnkiResponse
  { result :: Maybe Value
  , error  :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON AnkiResponse where
  parseJSON = withObject "AnkiResponse" $ \v ->
    MkAnkiResponse <$> v .: "result" <*> v .: "error"

startAnki :: IO ()
startAnki = void $ spawnProcess "anki" []

ankiRequest :: ToJSON a => a -> IO AnkiResponse
ankiRequest dat = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest "http://127.0.0.1:8765"
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS (encode dat)
        , requestHeaders = [("Content-Type", "application/json")]
        }
  resp <- httpLbs req' manager
  case eitherDecode (responseBody resp) of
    Left err -> Prelude.error err
    Right r  -> pure r

addNote :: ClozeNote -> IO AnkiResponse
addNote n = ankiRequest $ object
    [ "action"  .= ("addNote" :: Text)
    , "version" .= (6 :: Int)
    , "params"  .= object
        [ "note" .= object
            [ "deckName"  .= deckName n
            , "modelName" .= ("Cloze" :: Text)
            , "fields"    .= object
                [ "Text" .= text n
                , "Back Extra"  .= ("" :: Text)
                ]
            , "tags" .= tags n
            ]
        ]
    ]

