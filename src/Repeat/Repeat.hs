
module Repeat.Repeat where


pos::[Int]->Int->Int
pos xs y=  xs !!y 


					
					
					
--a)Implementa una función en Haskell que elimine de una lista de enteros aquellos 
--números múltiplo de x. > 
--  cribar [0,5,8,9,-9,6,0,85,-12,15] 2
--[5,9,-9,85,15]
--Se piden diferentes versiones de la misma función:
--- Con definición de listas por comprensión
cribar1::[Int]->Int->[Int]
cribar1 xs x= [n|n<-xs , n `mod` x/=0]


--- Con recursividad no final
cribar2::[Int]->Int->[Int]
cribar2  [] _ = []
cribar2 (x:xs) y= if (x`mod`y==0)then x:cribar2 xs y else cribar2 xs y

--- Con recursividad final o de cola
cribar3::[Int]->Int->[Int]
cribar3= cribarAux []

cribarAux::[Int] -> [Int] -> Int -> [Int]
cribarAux  r [] _ =r
cribarAux r (x:xs) n=  cribarAux xs (if x `rem` n /= 0 then r ++ [x] else r)n




--b) Dada la siguiente definición de función
--doble :: Int -> Int
--doble x = x + x
--¿Cómo cambiaría la definición utilizando expresiones lambda?
-- B
{-
(\x -> x + x)
-}



--c) Se pide una función en Haskell que dada una lista de números enteros obtenga un número 
-- entero con el resultado de calcular el doble de cada uno de los elementos
-- de la lista original y sumarlos todos. 
-- Se piden diferentes versiones de la misma función:
-- > sumaDobles [2,3,4]
--18
-- > sumaDobles [1,2,3]
--12

--- Con recursividad no final
sumaDobles::[Int]->Int 
sumaDobles [] = 0
sumaDobles (x:xs) =x*2 + sumaDobles xs

--- Con recursividad final o de cola 

sumaDoble::[Int]->Int	
sumaDoble = sumaDobleAux 0

sumaDobleAux::Int->[Int]->Int
sumaDobleAux r []=  r 
sumaDobleAux r (x:xs)=  sumaDobleAux (r + x * 2) xs


-- Utilizando expresiones lambda u orden superior (se puede hacer uso de la función 
-- predefinida de Haskell map).

sumaDobles''::[Int]->Int
sumaDobles'' xs =sum(map(\x->x+x) xs)






--d) Implementa una función que sume los cuadrados de los números pares contenidos en una 
-- lista de números enteros. Se piden dos versiones:

--a. Una versión que haga uso de las funciones de orden superior de listas map
--  y filter para definir la nueva función.



cuadrados::[Int]->[Int]
cuadrados xs= map (2^) (filter odd xs)


--b. Una versión que utilice la definición de listas por comprensión.
cuadrados'::[Int]->[Int]
cuadrados' xs=[x^2|x<-xs,odd x]





--e)Dada una lista de enteros, implementar una función para devolver tuplas
--  formadas por los elementos 
--  (sin repetir) de la lista, junto con la primera posición en la que aparecen. >
--  primeraAparicion [1,5,6,0,2,6,4,78,9,41,-9,8,-9,12,45,0] [(1,1),(5,2),(6,3),(0,4),(2,5),(4,7),(78,8),(9,9),(41,10),
--  (-9,11),(8,12),(12,14),(45,15)]

primeraAparicion::[Int]->[(Int,Int)]
primeraAparicion=primeraAparicionAux 1 [] []

primeraAparicionAux:: Int ->[Int]->[(Int,Int)]->[Int]->[(Int,Int)]
primeraAparicionAux  _ _ r [] =r
primeraAparicionAux i as ts (x:xs)= if x `elem` as then primeraAparicionAux(i+1) as ts xs else
														primeraAparicionAux(i+1)(as ++ [x])(ts++[(x,i)]) xs


--f) Implementar en Haskell una función que calcule el número de secuencias de ceros que hay en una lista de números. > ceros [0] > ceros[0,0]
--1 1 > ceros [0,1,0] > ceros [0,0,1,5,0,4,0,0,0,5]
--2 3

ceros::[Int]->Int
ceros= cerosAux True 0

cerosAux::Bool->Int->[Int]->Int
cerosAux  _ i [] = i
cerosAux  True i (x:xs) = if x == 0 then cerosAux  True i xs else cerosAux  False i xs 
cerosAux  False i (x:xs) = if x == 0 then cerosAux True (i+1) xs else cerosAux False i xs




--g) Implementar una función en Haskell que reciba una lista de números enteros y devuelva dos listas:
--  una con los elementos sin repetir y
--  otra con los elementos que están repetidos. > repeticiones [0,6,0,8,-2,-5,4,-2,6,98,71,2,0,5]
--([8,-5,4,98,71,2,5],[0,6,-2])

repeticiones::[Int]->[Int]
repeticiones = repeticionesAux [] 

repeticionesAux::[Int]->[Int]->[Int]
repeticionesAux r [] =r
repeticionesAux  ys (x:xs)  = if (x `elem` xs) then repeticionesAux  (x:ys) xs else repeticionesAux  ys xs



--h) Dada una lista de números enteros implementar una función que devuelva una lista con los n elementos mayores de la lista original.
-- > nmayores [8,4,-5,6,-1,0,2,6,-10,7] 4
--[8,7,6,6]
-- > nmayores [8,4,-5,6,-1,0,2,6,-10,7] 7
--[8,4,6,6,7,0,2]
-- > nmayores [8,4,-5,6,-1,0,2,6,-10,7] 11
--[8,4,-5,6,-1,0,2,6,-10,7]
 
nmayores::[Int]->Int->[Int]
nmayores=nmayoresAux []

nmayoresAux::[Int]->[Int]->Int->[Int]
nmayoresAux r [] _ = r
nmayoresAux ys (x:xs) i = if i/=0 then nmayoresAux ([x]++ys)  xs (i-1) else
								   if (x > minimum ys) then nmayoresAux ([y |y <-ys , y/=minimum ys]++[x]) xs i
								   else nmayoresAux ys xs i



--i) Implementa una función incluye en Haskell que reciba dos listas de números enteros y nos diga si la
--  primera de las listas está contenida en la segunda. Se dice que una lista está contenida en otra si los elementos 
-- de la primera aparecen dentro de la segunda, en el mismo orden y de forma consecutiva.
-- > incluye [] [4,5] > incluye [4,4,2] [5,4,4,5,4,4,2,9]
--True True
-- > incluye [4,4,2] [5,4,4,5,2,9] > incluye [4,5] []
--False False

incluye::[Int]->[Int]->Bool
incluye [] _= True
incluye _ []= False
incluye xs1 (x2:xs2)
	|incluyeAux xs1 (x2:xs2) =True
	|otherwise = incluye xs1 xs2

incluyeAux::[Int]->[Int]->Bool
incluyeAux [] _= True
incluyeAux _ [] = False
incluyeAux (x1:xs1)(x2:xs2)
	| x1==x2 = True
	| otherwise =False



--j) Dada una lista de enteros, se pide implementar una función que ordene dicha lista de 
-- menor a mayor utilizando un algoritmo de inserción. Dicho algoritmo de inserción consiste en recorrer la lista L,
--  insertando cada elemento L[i] en el lugar correcto entre los elementos ya ordenados L[1] ,...,L[i-1].
-- > ordenar [2,3,1]
--[1,2,3]
-- > ordenar [1,0,4,0,6,9]
--[0,0,1,4,6,9]

insertado::[Int]->[Int]
insertado = insertadoAux []

insertadoAux::[Int]->[Int]->[Int]
insertadoAux [] r = r
insertadoAux ys (x:xs) = insertadoAux(insertarOrdenado x ys) xs

insertarOrdenado:: Int->[Int]->[Int]
insertarOrdenado n(x:xs)= if n>x  then x:insertarOrdenado n xs else n:x:xs



--k) Implementa una función polimórfica en Haskell que reciba 2 listas y vaya cogiendo un elemento de la primera y dos de la segunda, 
--creando una lista final de ternas. En caso de que una de las dos listas se acabe, mostrará la lista de ternas construidas hasta ese momento. 
-- > mezclarEnTernas [4,5,8,90] [0,5,6,-9,8,-1,9,52,22]
--[(4,0,5),(5,6,-9),(8,8,-1),(90,9,52)]
-- > mezclarEnTernas [1,2,3] [5,6,7,8]
--[(1,5,6),(2,7,8)]
-- > mezclarEnTernas [1,2,3,4,5] "atropellado"
--[(1,'a','t'),(2,'r','o'),(3,'p','e'),(4,'l','l'),(5,'a','d')]
-- > mezclarEnTernas [True,False] [2.3,5.9,5.7]
--[(True,2.3,5.9)]

mezclarEnTernas::[x]->[y]->[(x,y,y)]
mezclarEnTernas = mezclarEnTernasAux []

mezclarEnTernasAux::[(x,y,y)]->[x]->[y]->[(x,y,y)]
mezclarEnTernasAux r [] _ =r
mezclarEnTernasAux r _ []=r
mezclarEnTernasAux r (x:xs)[y]=r
mezclarEnTernasAux r (x:xs)(y1:y2:ys)= mezclarEnTernasAux(r++ [(x,y1,y2)]) xs  ys



--l) Se pide una función polimórfica en Haskell que dado un elemento y una lista añada dicho elemento al final de la lista.
-- > alFinal 3 [1,2,6,7]
--[1,2,6,7,3]
-- > alFinal True [False,False]
--[False,False,True]
-- > alFinal 'k' "casita"
--"casitak"
alFinal:: a->[a]->[a]
alFinal n []=[n]
alFinal x ys= ys++[x]    


--m) Mediante la programación de orden superior se pide implementar una de las funciones predefinidas en la librería estándar
--  de Haskell: 
-- la función zipWith. Esta función recibe como parámetros una función y dos listas y une ambas listas aplicado la función entre
--  los correspondientes parámetros.
-- > zipWith' max [6,3,2,1] [7,3,1,5]
--[7,3,2,5]
-- > zipWith' (++) ["hola ", "ciao ", "hi "] ["pepe", "ciao", "peter"]
--["hola pepe","ciao ciao","hi peter"]
--ghci> zipWith' (*) (replicate 5 2) [1..]
--[2,4,6,8,10]
-- > zipWith' (zipWith' (*)) [[1,2,3],[3,5,6]] [[3,2,2],[3,4,5]]
--[[3,4,6],[9,20,30]]
-- > zipWith’ crearTupla [1,2,3] "casita"
--[(1,'c'),(2,'a'),(3,'s')]

zipWith'::(a->b->c)->[a]->[b]->[c]
zipWith' f [][]=[]
zipWith' f [] _=[]
zipWith' f _ []=[]
zipWith' f(n1:ns1)(n2:ns2)=[f n1 n2]  ++ zipWith' f ns1 ns2


--(Suponiendo que la función crearTupla tiene la siguiente definición:
--crearTupla :: a-> b-> (a,b)
--crearTupla x y = (x,y)
--)
	

	
	

--n) Define una función polimórfica que sea capaz de invertir 
-- los elementos de una lista. Se piden diferentes versiones:
-- > reverse' [1,2,3]
--[3,2,1]
-- > reverse' "casa"
--"asac"


--- Con recursividad no final
reverse'::[a]->[a]
reverse' []=  []
reverse'(x:xs)= reverse' xs ++ [x]

--- Con recursividad de cola o final

reversa::[a]->[a]
reversa = reverseAux[]

reverseAux::[a]->[a]->[a]
reverseAux r []=r
reverseAux ys (x:xs)=  reverseAux ([x]++ys) xs
--- Utilizando la función de orden superior foldr

reve::[a]->[a]
reve = foldr(\n ns->ns ++[n])[]

--o) Define una función polimórfica que sea capaz de invertir los elementos de una lista de listas.
-- > reverse'' [[1,2,3],[3,4,5]]
--[[5,4,3],[3,2,1]]
-- > reverse'' ["pepe", "casa", "patio"]
--["oitap","asac","epep"]
rever::[[a]]->[[a]]
rever=foldr(\n ns->ns ++[reve n])[]
--p) Implementar la función predefinida de la librería estándar flip. 
-- Esta función lo que hace es recibir una función y
-- devolver otra función que es idéntica a la función original, salvo que intercambia 
-- los dos primeros parámetros.
-- > flip' zip [1,2,3] "casa"
--[('c',1),('a',2),('s',3)]
-- > flip' (+) 3 4
--7
-- > flip' (++) "casa" "pollo"
--"pollocasa"
flip'::(a->b->c)->(b->a->c)
flip' f x y = f y x


--q) Implementar la función polimórfica predefinida de la librería 
-- estándar map. Esta función lo que hace es recibir una función 
-- y una lista y devuelve la lista resultante de aplicar la función a cada
--  elemento de la lista original.
-- > map (3*) [1,2,3]
--[3,6,9]
-- > map doble [1,2,3]
--[2,4,6]
-- > map not [True,F
map'::(a->a)->[a]->[a]
map' = mapAux []

mapAux ::[a]->(a->a)->[a]->[a]
mapAux r _ [] = r
mapAux ys f (x:xs)= mapAux (ys ++[f x]) f xs
-- EJERCICIOS foldr foldl
--1. Analiza cual sería el resultado de aplicar las siguientes funciones sobre la lista [2,3,5]
--a. foldr (\ a b-> a:b)[]  [2,3,5]

--b. foldr (\a b->[a]:b)[] [2,3,5]

--c. foldl (\ a b-> if b== 2 then 0:a else b:a)[]

--d. foldl (\ a b-> a:b)[]

--2. Desarrolla la ejecución de la función incognita para ver qué haría sobre la lista de números enteros incluida en su invocación
--incognita l = foldl (\ (y:z:xs) x -> if odd x then [y++[x],z] else [y,z++[x]])[[],[]] [14,5,8,7,9,16]
--3. Desarrolla diferentes funciones que hagan uso de alguna de las versiones de la función fold y que:
--a. Reciba una lista de enteros y devuelva la suma de sus dobles.
--b. Reciba una lista de enteros y devuelva la suma de sus cuadrados.
--c. Reciba una lista de enteros y un entero y lo inserte al final de dicha lista.
--d. Reciba una lista y un número entero y devuelva dicha lista eliminando las apariciones de ese número entero

-- Guion
