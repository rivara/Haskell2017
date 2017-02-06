
module Ejercicios.Foldr_Foldl where
Ejercicios con la instrucción fold
1. Analiza cual sería el resultado de aplicar las siguientes funciones sobre la lista [2,3,5]
a. foldr (\ a b-> a:b)[]
b. foldr (\a b->[a]:b)[]
c. foldl (\ a b-> if b== 2 then 0:a else b:a)[]
d. foldl (\ a b-> a:b)[]
2. Desarrolla la ejecución de la función incognita para ver qué haría sobre la lista de números enteros incluida en su invocación
incognita l = foldl (\ (y:z:xs) x -> if odd x then [y++[x],z] else [y,z++[x]])[[],[]] [14,5,8,7,9,16]
3. Desarrolla diferentes funciones que hagan uso de alguna de las versiones de la función fold y que:
a. Reciba una lista de enteros y devuelva la suma de sus dobles.
b. Reciba una lista de enteros y devuelva la suma de sus cuadrados.
c. Reciba una lista de enteros y un entero y lo inserte al final de dicha lista.
d. Reciba una lista y un número entero y devuelva