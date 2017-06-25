
module Examenes.AntiguaOrdinaria2015 where
-- 3A (Error)
 --class Tabular a where {
--	tab:: a-> a
-- }
 
--instance Tabular a where{
--	tab a= a++"/t" 
-- }

--E5
--A
p :: [a]-> [[a]]
p [] = [[]]
p (x:xs) = [] :map (x:) (p xs)
--B
concat' ::[[a]]->[a]
concat' xs= reverse(foldl(\a b -> b++a)[] xs)
-- concat'[[1],[2],[3]]

-- Ejercicio 5 Incompleto
data Arbol a= AV |Rama(Arbol a) a (Arbol a) deriving Show

abb::(Ord a)=>Arbol a -> a ->Arbol a
abb (Rama AV x AV) n= if(x>n)then (Rama (Rama AV n AV) x AV) else (Rama (Rama AV n AV)  x AV)
abb (Rama hizq x hder) n= if(x>n)then abb hizq n else abb hder n
-- abb (Rama AV 3(Rama AV 2 AV)) 3



-- prueba