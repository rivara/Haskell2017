
module Ejercicios.Foldr_Foldl where
--Ejercicios con la instrucción fold
--1. Analiza cual sería el resultado de aplicar las siguientes funciones sobre la lista [2,3,5]
--a. foldr (\ a b-> a:b)[]
--b. foldr (\a b->[a]:b)[]
--c. foldl (\ a b-> if b== 2 then 0:a else b:a)[]
--d. foldl (\ a b-> a:b)[]

-- a. [2,3,5]
-- b. [[2],[3],[5]]
-- c. [5,3,0]
-- d. Error

--2. Desarrolla la ejecución de la función incognita para ver qué haría sobre la lista de números enteros incluida en su invocación
--incognita l = foldl (\ (y:z:xs) x -> if odd x then [y++[x],z] else [y,z++[x]])[[],[]] [14,5,8,7,9,16]

incognita l = foldl (\ (y:z:xs) x -> if odd x then [y++[x],z] else [y,z++[x]])[[],[]] [14,5,8,7,9,16]

--3. Desarrolla diferentes funciones que hagan uso de alguna de las versiones de la función fold y que:
--a. Reciba una lista de enteros y devuelva la suma de sus dobles.

dobles:: [Int]->[Int]
dobles = foldr(\a b-> 2*a:b )[]


dobles':: [Int]->[Int]
dobles'= foldl(\b a-> 2*a:b )[]


--b. Reciba una lista de enteros y devuelva la suma de sus cuadrados.

cuadrados::[Int]->Int
cuadrados= foldl(\a b-> (b^2)+a)0

cuadrados'::[Int]->Int
cuadrados'=foldr(\a b-> (a^2)+b)0

--c. Reciba una lista de enteros y un entero y lo inserte al final de dicha lista.
fin::[Int]->Int->[Int]
fin xs n= foldr(\a b-> a:b)[n] xs

fin'::[Int]->Int->[Int]
fin' xs n= foldl(\a b-> b:a )[n]xs


--d. Reciba una lista y un número entero y devuelva dicha lista eliminando las apariciones de ese número entero.

aparece::[Int]->Int->[Int]
aparece xs n= foldr(\a b->  if a==n then b else a:b) [] xs

aparece'::[Int]->Int->[Int]
aparece' xs n= foldl(\a b-> if b==n then a else b:a)[] xs



