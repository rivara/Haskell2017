
module Ejercicios.Ejercicio2 where
--a)Implementa una funci�n en Haskell que elimine de una lista de enteros aquellos n�meros m�ltiplo de x. > 
--  cribar [0,5,8,9,-9,6,0,85,-12,15] 2
--[5,9,-9,85,15]
--Se piden diferentes versiones de la misma funci�n:
--- Con definici�n de listas por comprensi�n
cribar::[Int]->Int->[Int]
cribar xs y= [x|x<-xs , x`rem`y /=0] 

--- Con recursividad no final
cribar'::[Int]->Int->[Int]
cribar'  [] _ = []
cribar' (x:xs)  y=  if ( x`rem`y /=0) then x:cribar' xs y else cribar' xs y

--- Con recursividad final o de cola

cribar'' :: [Int] -> Int -> [Int]
cribar''  = cribarAux []

cribarAux::[Int] -> [Int] -> Int -> [Int]
cribarAux r [] _ =r
cribarAux r (x:xs) n=  cribarAux xs (if x `rem` n /= 0 then r ++ [x] else r) n





--b) Dada la siguiente definici�n de funci�n
--doble :: Int -> Int
--doble x = x + x
--�C�mo cambiar�a la definici�n utilizando expresiones lambda?
-- B
{-
(\x -> x + x)
-}

--c) Se pide una funci�n en Haskell que dada una lista de n�meros enteros obtenga un n�mero 
-- entero con el resultado de calcular el doble de cada uno de los elementos de la lista original y sumarlos todos. 
-- Se piden diferentes versiones de la misma funci�n:

--- Con recursividad no final
sumaDobles::[Int]->Int
sumaDobles [] = 0
sumaDobles (x:xs) =x*2 + sumaDobles xs   


--- Con recursividad final o de cola

sumaDobles'::[Int]->Int
sumaDobles'= sumaDoblesAux 0

sumaDoblesAux::Int->[Int]->Int
sumaDoblesAux r[]= r
sumaDoblesAux r(x:xs) = sumaDoblesAux (r + x * 2)  xs			


--- Utilizando expresiones lambda u orden superior (se puede hacer uso de la funci�n predefinida de Haskell map).
-- > sumaDobles [2,3,4]
--18
-- > sumaDobles [1,2,3]
--12

sumaDobles''::[Int]->Int
sumaDobles'' xs =sum(map(\x->x+x) xs)

--d) Implementa una funci�n que sume los cuadrados de los n�meros pares contenidos en una lista de n�meros enteros. Se piden dos versiones:

--a. Una versi�n que haga uso de las funciones de orden superior de listas map y filter para definir la nueva funci�n.
cuadrados::[Int]->[Int]
cuadrados xs= map (2^) (filter odd xs)

--b. Una versi�n que utilice la definici�n de listas por comprensi�n.
cuadrados'::[Int]->[Int]
cuadrados' xs= [x^2|x<-xs, odd x]

--e)Dada una lista de enteros, implementar una funci�n para devolver tuplas formadas por los elementos 
--  (sin repetir) de la lista, junto con la primera posici�n en la que aparecen. >
--  primeraAparicion [1,5,6,0,2,6,4,78,9,41,-9,8,-9,12,45,0] [(1,1),(5,2),(6,3),(0,4),(2,5),(4,7),(78,8),(9,9),(41,10),
--  (-9,11),(8,12),(12,14),(45,15)]

primeraAparicion::[Int]->[(Int,Int)]
primeraAparicion = primeraAparicionAux 1 [] [] -- �ndice Lista_Aparecidos Lista_Tuplas_Resultado Lista_Original (Currificaci�n)


primeraAparicionAux :: Int->[Int]->[(Int,Int)]->[Int]->[(Int,Int)]
primeraAparicionAux _ _ r [] = r
primeraAparicionAux i as ts (x:xs)= if x `elem` as then primeraAparicionAux (i+1) as ts xs else 
													 primeraAparicionAux (i+1)(as ++ [x])(ts ++[(x,i)]) xs
-- **********************************

--f) Implementar en Haskell una funci�n que calcule el n�mero de secuencias de ceros que hay en una lista de n�meros. > ceros [0] > ceros[0,0]
--1 1 > ceros [0,1,0] > ceros [0,0,1,5,0,4,0,0,0,5]
--2 3
ceros::[Int]->Int
ceros = cerosAux True 0
 
cerosAux::Bool->Int->[Int]->Int
cerosAux _  i []= i
cerosAux True i (x:xs)= if x==0 then cerosAux True i  xs  else cerosAux  False i xs 
cerosAux False i (x:xs) = if x==0 then cerosAux True (i+1) xs  else cerosAux False i xs 

--g) Implementar una funci�n en Haskell que reciba una lista de n�meros enteros y devuelva dos listas: una con los elementos sin repetir y
--  otra con los elementos que est�n repetidos. > repeticiones [0,6,0,8,-2,-5,4,-2,6,98,71,2,0,5]
--([8,-5,4,98,71,2,5],[0,6,-2])

repeticiones::[Int]->[Int]
repeticiones = repeticionesAux []

repeticionesAux ::[Int]->[Int]->[Int]
repeticionesAux r []= r
repeticionesAux  ys (x:xs)= if x `elem` xs then repeticionesAux (ys++[x]) xs else repeticionesAux ys xs


--h) Dada una lista de n�meros enteros implementar una funci�n que devuelva una lista con los n elementos mayores de la lista original.
-- > nmayores [8,4,-5,6,-1,0,2,6,-10,7] 4
--[8,7,6,6]
-- > nmayores [8,4,-5,6,-1,0,2,6,-10,7] 7
--[8,4,6,6,7,0,2]
-- > nmayores [8,4,-5,6,-1,0,2,6,-10,7] 11
--[8,4,-5,6,-1,0,2,6,-10,7]

nmayores:: [Int]->Int->[Int]
nmayores= nmayoresAux []

nmayoresAux ::[Int]->[Int]->Int->[Int]
nmayoresAux r [] _ = r
nmayoresAux ys (x:xs) n= if n/=0 then nmayoresAux ([x]++ys) xs (n-1) else
						 if (x > minimum ys) then nmayoresAux ([y |y <- ys, y/= minimum ys] ++ [x]) xs n else  nmayoresAux ys xs n 
						 
						 
--i) Implementa una funci�n incluye en Haskell que reciba dos listas de n�meros enteros y nos diga si la
--  primera de las listas est� contenida en la segunda. Se dice que una lista est� contenida en otra si los elementos 
-- de la primera aparecen dentro de la segunda, en el mismo orden y de forma consecutiva.
-- > incluye [] [4,5] > incluye [4,4,2] [5,4,4,5,4,4,2,9]
--True True
-- > incluye [4,4,2] [5,4,4,5,2,9] > incluye [4,5] []
--False False
incluye :: [Int]->[Int]->Bool
incluye [] _=True
incluye _ []=False
incluye xs1 (x2:xs2)
	|incluyeAux xs1 (x2:xs2) = True
	|otherwise = incluye xs1 xs2

incluyeAux::[Int]->[Int]->Bool
incluyeAux[] _=True
incluyeAux _[]= False
incluyeAux(x1:xs1)(x2:xs2)
	|x1==x2 = incluyeAux xs1 xs2
	|otherwise =False
 


--j) Dada una lista de enteros, se pide implementar una funci�n que ordene dicha lista de 
-- menor a mayor utilizando un algoritmo de inserci�n. Dicho algoritmo de inserci�n consiste en recorrer la lista L,
--  insertando cada elemento L[i] en el lugar correcto entre los elementos ya ordenados L[1] ,...,L[i-1].
-- > ordenar [2,3,1]
--[1,2,3]
-- > ordenar [1,0,4,0,6,9]
--[0,0,1,4,6,9]

-- Version Ricardo
ordenar::[Int]->[Int]
ordenar = ordenarAux[]

ordenarAux ::[Int]->[Int]->[Int]
ordenarAux r []= r 
ordenarAux ys (x:xs) = ordenarAux (insertarOrdenado x ys) xs

insertarOrdenado::Int->[Int]->[Int]
insertarOrdenado n []=[n]
insertarOrdenado n(x:xs)= if n>x then x:insertarOrdenado n xs else n:x:xs	



--k) Implementa una funci�n polim�rfica en Haskell que reciba 2 listas y vaya cogiendo un elemento de la primera y dos de la segunda, 
--creando una lista final de ternas. En caso de que una de las dos listas se acabe, mostrar� la lista de ternas construidas hasta ese momento. 
-- > mezclarEnTernas [4,5,8,90] [0,5,6,-9,8,-1,9,52,22]
--[(4,0,5),(5,6,-9),(8,8,-1),(90,9,52)]
-- > mezclarEnTernas [1,2,3] [5,6,7,8]
--[(1,5,6),(2,7,8)]
-- > mezclarEnTernas [1,2,3,4,5] "atropellado"
--[(1,'a','t'),(2,'r','o'),(3,'p','e'),(4,'l','l'),(5,'a','d')]
-- > mezclarEnTernas [True,False] [2.3,5.9,5.7]
--[(True,2.3,5.9)]


mezclarEnTernas::[a]->[b]->[(a,b,b)]
mezclarEnTernas =mezclarEnTernasAux[]

mezclarEnTernasAux::[(a,b,b)]->[a]->[b]->[(a,b,b)]
mezclarEnTernasAux r [] _= r
mezclarEnTernasAux r _ []= r
mezclarEnTernasAux r (x:xs)[y]= r
mezclarEnTernasAux r (x:xs)(y1:y2:ys)= mezclarEnTernasAux (r ++ [(x,y1,y2)]) xs ys 

--l) Se pide una funci�n polim�rfica en Haskell que dado un elemento y una lista a�ada dicho elemento al final de la lista.
-- > alFinal 3 [1,2,6,7]
--[1,2,6,7,3]
-- > alFinal True [False,False]
--[False,False,True]
-- > alFinal 'k' "casita"
--"casitak"

alFinal:: a->[a]->[a]
alFinal n []=[n] 
alFinal x ys= ys++[x] 



--m) Mediante la programaci�n de orden superior se pide implementar una de las funciones predefinidas en la librer�a est�ndar de Haskell: 
-- la funci�n zipWith. Esta funci�n recibe como par�metros una funci�n y dos listas y une ambas listas aplicado la funci�n entre
--  los correspondientes par�metros.
-- > zipWith' max [6,3,2,1] [7,3,1,5]
--[7,3,2,5]
-- > zipWith' (++) ["hola ", "ciao ", "hi "] ["pepe", "ciao", "peter"]
--["hola pepe","ciao ciao","hi peter"]
--ghci> zipWith' (*) (replicate 5 2) [1..]
--[2,4,6,8,10]
-- > zipWith' (zipWith' (*)) [[1,2,3],[3,5,6]] [[3,2,2],[3,4,5]]
--[[3,4,6],[9,20,30]]
-- > zipWith� crearTupla [1,2,3] "casita"
--[(1,'c'),(2,'a'),(3,'s')]
--(Suponiendo que la funci�n crearTupla tiene la siguiente definici�n:
--crearTupla :: a-> b-> (a,b)
--crearTupla x y = (x,y)
--)

crearTupla :: a-> b-> (a,b)
crearTupla x y = (x,y)

zipWith':: (a->b->c)->[a]->[b]->[c]
zipWith' f [] []=[]
zipWith' f [] _	=[]
zipWith' f _ [] =[]
zipWith' f (n1:ns1)(n2:ns2)=[f n1 n2] ++ zipWith' f ns1 ns2




--n) Define una funci�n polim�rfica que sea capaz de invertir los elementos de una lista. Se piden diferentes versiones:
-- > reverse' [1,2,3]
--[3,2,1]
-- > reverse' "casa"
--"asac"


--- Con recursividad no final
reverse'::[a]->[a]
reverse' [] = []
reverse'(x:xs)=  reverse' xs ++[x]


--- Con recursividad de cola o final

reverse''::[a]->[a]
reverse'' = reverseAux[]

reverseAux::[a]->[a]->[a]
reverseAux r []= r
reverseAux r (x:xs)=  reverseAux ([x]++r) xs  --(x:r)

--- Utilizando la funci�n de orden superior foldr
reverse'''::[a]->[a]
reverse''' = foldr (\n ns->ns ++ [n]) []


--o) Define una funci�n polim�rfica que sea capaz de invertir los elementos de una lista de listas.
-- > reverse'' [[1,2,3],[3,4,5]]
--[[5,4,3],[3,2,1]]
-- > reverse'' ["pepe", "casa", "patio"]
--["oitap","asac","epep"]

reve::[[a]]->[[a]]
reve = foldr(\n ns->ns ++[reverse''' n])[]



--p) Implementar la funci�n predefinida de la librer�a est�ndar flip. Esta funci�n lo que hace es recibir una funci�n y
-- devolver otra funci�n que es id�ntica a la funci�n original, salvo que intercambia los dos primeros par�metros.
-- > flip' zip [1,2,3] "casa"
--[('c',1),('a',2),('s',3)]
-- > flip' (+) 3 4
--7
-- > flip' (++) "casa" "pollo"
--"pollocasa"


flip':: (a->b->c)->(b->a->c)
flip' f x y=f y x



--q) Implementar la funci�n polim�rfica predefinida de la librer�a est�ndar map. Esta funci�n lo que hace es recibir una funci�n 
-- y una lista y devuelve la lista resultante de aplicar la funci�n a cada elemento de la lista original.
-- > map (3*) [1,2,3]
--[3,6,9]
-- > map doble [1,2,3]
--[2,4,6]
-- > map not [True,False]
--[False,True]

mapt:: (a->a)->[a]->[a]
mapt = mapAux []

mapAux::[a]->(a->a)->[a]->[a]
mapAux r _ 	[]=r 
mapAux r f (x:xs)= mapAux (r++[f x]) f xs



