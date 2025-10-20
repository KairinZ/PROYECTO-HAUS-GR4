module Memory
  ( Memory
  , MemoryValue(..)
  , emptyMemory
  , set, get
  ) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Geometry (Point)

-- Diccionario de memoria: clave -> valor
type Memory = Map String MemoryValue

-- Valores que se pueden guardar
data MemoryValue
  = MemInt Int
  | MemString String
  | MemBool Bool
  | MemPoint Point -- coordenadas simples
  | MemFloat Float -- Permite guardar valores de punto flotante.
  deriving (Show, Eq)

-- Memoria vacía
emptyMemory :: Memory
emptyMemory = M.empty

-- Guardar un valor
set :: String -> MemoryValue -> Memory -> Memory
set = M.insert

-- Recuperar un valor
-- CORRECCIÓN: Se ha eliminado el parámetro extra "MemoryValue" de la firma.
get :: String -> Memory -> Maybe MemoryValue
get = M.lookup

