
module Examenes.Extraordinaria2012 where
-- recorrido inorden -- preorden -postorden

data Arbol a=AV|Rama (Arbol a) a (Arbol a) deriving (Show,Eq)



preorden :: Arbol a -> [a]
preorden AV = []
preorden (Rama hizq r hder) = [r] ++ preorden hizq ++ preorden hder -- No se puede aplicar Recursividad Final porque la Recursividad es Múltiple

-- preorden (Rama (Rama AV 2 ((Rama AV 4 AV))) 3 (Rama AV 6 AV))

postorden :: Arbol a->[a]
postorden AV =[]
postorden(Rama hizq r hder)=  preorden hizq ++ preorden hder ++[r] 

-- postorden (Rama (Rama AV 2 ((Rama AV 4 AV))) 3 (Rama AV 6 AV))

inorden :: Arbol a->[a]
inorden AV =[]
inorden(Rama hizq r hder)=  inorden hizq ++[r]++ preorden hder 

-- inorden (Rama (Rama AV 2 ((Rama AV 4 AV))) 3 (Rama AV 6 AV))