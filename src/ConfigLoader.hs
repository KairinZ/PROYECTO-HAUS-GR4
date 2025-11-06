{-# LANGUAGE RecordWildCards #-}

-- ===========================================================
-- ConfigLoader.hs
-- ===========================================================
-- Módulo para cargar y parsear el archivo de configuración
-- config.txt que especifica los parámetros del torneo.
-- ===========================================================

module ConfigLoader
  ( TournamentConfig(..)
  , loadConfig
  , defaultConfig
  ) where

import Entities (AIType(..))
import GameTypes (GameConfig(..), BotConfig(..))
import Data.List (isPrefixOf)
import Data.Char (isSpace)
import System.IO
import Control.Exception (catch, IOException)
import Data.Maybe (fromMaybe)

-- | Configuración del torneo leída del archivo
data TournamentConfig = TournamentConfig
  { configNumBots              :: Int
  , configBotTypes             :: [AIType]
  , configAreaWidth            :: Int
  , configAreaHeight           :: Int
  , configMaxTournamentDuration :: Float
  , configNumTournaments        :: Int
  } deriving (Show)

-- | Configuración por defecto si no se puede cargar el archivo
defaultConfig :: TournamentConfig
defaultConfig = TournamentConfig
  { configNumBots              = 2
  , configBotTypes             = [Hunter { hunterDetectionRange = 400.0 }, Evasive { evasiveDetectionRange = 200.0 }]
  , configAreaWidth            = 800
  , configAreaHeight           = 600
  , configMaxTournamentDuration = 300.0
  , configNumTournaments       = 5
  }

-- | Carga la configuración desde el archivo config.txt
loadConfig :: IO TournamentConfig
loadConfig = do
  result <- tryLoadConfig
  case result of
    Just cfg -> return cfg
    Nothing  -> return defaultConfig

-- | Intenta cargar el archivo de configuración
tryLoadConfig :: IO (Maybe TournamentConfig)
tryLoadConfig = do
  content <- readFile "config.txt" `catch` (\(_ :: IOException) -> return "")
  if null content
    then return Nothing
    else do
      let lines = filter (not . null) $ map trim $ splitLines content
      return $ parseConfig lines

-- | Divide una cadena en líneas
splitLines :: String -> [String]
splitLines = lines

-- | Parsea las líneas del archivo de configuración
parseConfig :: [String] -> Maybe TournamentConfig
parseConfig lines = Just $ TournamentConfig
  { configNumBots              = fromMaybe 2 $ lookupInt "num_bots" lines
  , configBotTypes             = fromMaybe [Hunter { hunterDetectionRange = 400.0 }, Evasive { evasiveDetectionRange = 200.0 }] $ lookupBotTypes "bot_types" lines
  , configAreaWidth            = fromMaybe 800 $ lookupInt "area_width" lines
  , configAreaHeight           = fromMaybe 600 $ lookupInt "area_height" lines
  , configMaxTournamentDuration = fromMaybe 300.0 $ lookupFloat "max_tournament_duration" lines
  , configNumTournaments       = fromMaybe 5 $ lookupInt "num_tournaments" lines
  }

-- | Busca un valor entero en las líneas de configuración
lookupInt :: String -> [String] -> Maybe Int
lookupInt key lines = do
  line <- findLine key lines
  let value = dropWhile (/= ':') line
  if null value then Nothing else readInt $ trim $ drop 1 value

-- | Busca un valor flotante en las líneas de configuración
lookupFloat :: String -> [String] -> Maybe Float
lookupFloat key lines = do
  line <- findLine key lines
  let value = dropWhile (/= ':') line
  if null value then Nothing else readFloat $ trim $ drop 1 value

-- | Busca los tipos de bots en las líneas de configuración
lookupBotTypes :: String -> [String] -> Maybe [AIType]
lookupBotTypes key lines = do
  line <- findLine key lines
  let value = dropWhile (/= ':') line
  if null value then Nothing else parseBotTypes $ trim $ drop 1 value

-- | Encuentra una línea que comienza con la clave dada
findLine :: String -> [String] -> Maybe String
findLine key = foldr (\line acc -> if key `isPrefixOf` line then Just line else acc) Nothing

-- | Parsea una lista de tipos de bots separados por comas
parseBotTypes :: String -> Maybe [AIType]
parseBotTypes str = Just $ map parseBotType $ filter (not . null) $ map trim $ splitComma str

-- | Divide una cadena por comas
splitComma :: String -> [String]
splitComma [] = []
splitComma str =
  let (before, remainder) = break (== ',') str
  in before : case remainder of
    [] -> []
    (_:after) -> splitComma after

-- | Parsea un tipo de bot individual
parseBotType :: String -> AIType
parseBotType s
  | trim s == "Hunter" = Hunter { hunterDetectionRange = 400.0 }
  | trim s == "Evasive" = Evasive { evasiveDetectionRange = 200.0 }
  | otherwise = Hunter { hunterDetectionRange = 400.0 }  -- Por defecto

-- | Lee un entero de una cadena
readInt :: String -> Maybe Int
readInt s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

-- | Lee un flotante de una cadena
readFloat :: String -> Maybe Float
readFloat s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

-- | Elimina espacios en blanco al inicio y final
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

