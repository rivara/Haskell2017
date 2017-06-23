
module Examenes.Extraordinaria2012 where
-- recorrido inorden -- preorden -postorden

data Arbol a=AV|Rama (Arbol a) a (Arbol a) deriving (Show,Eq)
