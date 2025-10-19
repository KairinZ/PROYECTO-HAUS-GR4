module Memory
  ( Memory
  , MemoryValue(..)
  , emptyMemory
  , set, get
  ) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import Geometry (Point, Vector)

-- Diccionario de memoria: clave -> valor
type Memory = Map String MemoryValue

-- Valores que se pueden guardar
data MemoryValue
  = MemInt Int
  | MemString String
  | MemBool Bool
  | MemPoint Point -- coordenadas simples
  deriving (Show, Eq)

-- Memoria vacía
emptyMemory :: Memory
emptyMemory = M.empty

-- Guardar un valor
set :: String -> MemoryValue -> Memory -> Memory
set = M.insert

-- Recuperar un valor
get :: String -> Memory -> Maybe MemoryValue
get = M.lookup
