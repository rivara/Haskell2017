
module Repeat.Repeat where


pos::[Int]->Int->Int
pos xs y=  xs !!y 


					
					
					
--a)Implementa una funci�n en Haskell que elimine de una lista de enteros aquellos 
--n�meros m�ltiplo de x. > 
--  cribar [0,5,8,9,-9,6,0,85,-12,15] 2
--[5,9,-9,85,15]
--Se piden diferentes versiones de la misma funci�n:
--- Con definici�n de listas por comprensi�n
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




--b) Dada la siguiente definici�n de funci�n
--doble :: Int -> Int
--doble x = x + x
--�C�mo cambiar�a la definici�n utilizando expresiones lambda?
-- B
{-
(\x -> x + x)
-}



--c) Se pide una funci�n en Haskell que dada una lista de n�meros enteros obtenga un n�mero 
-- entero con el resultado de calcular el doble de cada uno de los elementos
-- de la lista original y sumarlos todos. 
-- Se piden diferentes versiones de la misma funci�n:
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


-- Utilizando expresiones lambda u orden superior (se puede hacer uso de la funci�n 
-- predefinida de Haskell map).

sumaDobles''::[Int]->Int
sumaDobles'' xs =sum(map(\x->x+x) xs)






--d) Implementa una funci�n que sume los cuadrados de los n�meros pares contenidos en una 
-- lista de n�meros enteros. Se piden dos versiones:

--a. Una versi�n que haga uso de las funciones de orden superior de listas map
--  y filter para definir la nueva funci�n.



cuadrados::[Int]->[Int]
cuadrados xs= map (2^) (filter odd xs)


--b. Una versi�n que utilice la definici�n de listas por comprensi�n.
cuadrados'::[Int]->[Int]
cuadrados' xs=[x^2|x<-xs,odd x]





--e)Dada una lista de enteros, implementar una funci�n para devolver tuplas
--  formadas por los elementos 
--  (sin repetir) de la lista, junto con la primera posici�n en la que aparecen. >
--  primeraAparicion [1,5,6,0,2,6,4,78,9,41,-9,8,-9,12,45,0] [(1,1),(5,2),(6,3),(0,4),(2,5),(4,7),(78,8),(9,9),(41,10),
--  (-9,11),(8,12),(12,14),(45,15)]

primeraAparicion::[Int]->[(Int,Int)]
primeraAparicion=primeraAparicionAux 1 [] []

primeraAparicionAux:: Int ->[Int]->[(Int,Int)]->[Int]->[(Int,Int)]
primeraAparicionAux  _ _ r [] =r
primeraAparicionAux i as ts (x:xs)= if x `elem` as then primeraAparicionAux(i+1) as ts xs else
														primeraAparicionAux(i+1)(as ++ [x])(ts++[(x,i)]) xs


--f) Implementar en Haskell una funci�n que calcule el n�mero de secuencias de ceros que hay en una lista de n�meros. > ceros [0] > ceros[0,0]
--1 1 > ceros [0,1,0] > ceros [0,0,1,5,0,4,0,0,0,5]
--2 3

ceros::[Int]->Int
ceros= cerosAux True 0

cerosAux::Bool->Int->[Int]->Int
cerosAux  _ i [] = i
cerosAux  True i (x:xs) = if x == 0 then cerosAux  True i xs else cerosAux  False i xs 
cerosAux  False i (x:xs) = if x == 0 then cerosAux True (i+1) xs else cerosAux False i xs




--g) Implementar una funci�n en Haskell que reciba una lista de n�meros enteros y devuelva dos listas:
--  una con los elementos sin repetir y
--  otra con los elementos que est�n repetidos. > repeticiones [0,6,0,8,-2,-5,4,-2,6,98,71,2,0,5]
--([8,-5,4,98,71,2,5],[0,6,-2])

repeticiones::[Int]->[Int]
repeticiones = repeticionesAux [] 

repeticionesAux::[Int]->[Int]->[Int]
repeticionesAux r [] =r
repeticionesAux  ys (x:xs)  = if (x `elem` xs) then repeticionesAux  (x:ys) xs else repeticionesAux  ys xs



--h) Dada una lista de n�meros enteros implementar una funci�n que devuelva una lista con los n elementos mayores de la lista original.
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



--i) Implementa una funci�n incluye en Haskell que reciba dos listas de n�meros enteros y nos diga si la
--  primera de las listas est� contenida en la segunda. Se dice que una lista est� contenida en otra si los elementos 
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



--j) Dada una lista de enteros, se pide implementar una funci�n que ordene dicha lista de 
-- menor a mayor utilizando un algoritmo de inserci�n. Dicho algoritmo de inserci�n consiste en recorrer la lista L,
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

mezclarEnTernas::[x]->[y]->[(x,y,y)]
mezclarEnTernas = mezclarEnTernasAux []

mezclarEnTernasAux::[(x,y,y)]->[x]->[y]->[(x,y,y)]
mezclarEnTernasAux r [] _ =r
mezclarEnTernasAux r _ []=r
mezclarEnTernasAux r (x:xs)[y]=r
mezclarEnTernasAux r (x:xs)(y1:y2:ys)= mezclarEnTernasAux(r++ [(x,y1,y2)]) xs  ys



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


--m) Mediante la programaci�n de orden superior se pide implementar una de las funciones predefinidas en la librer�a est�ndar
--  de Haskell: 
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

zipWith'::(a->b->c)->[a]->[b]->[c]
zipWith' f [][]=[]
zipWith' f [] _=[]
zipWith' f _ []=[]
zipWith' f(n1:ns1)(n2:ns2)=[f n1 n2]  ++ zipWith' f ns1 ns2


--(Suponiendo que la funci�n crearTupla tiene la siguiente definici�n:
--crearTupla :: a-> b-> (a,b)
--crearTupla x y = (x,y)
--)
	

	
	

--n) Define una funci�n polim�rfica que sea capaz de invertir 
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
--- Utilizando la funci�n de orden superior foldr

reve::[a]->[a]
reve = foldr(\n ns->ns ++[n])[]

--o) Define una funci�n polim�rfica que sea capaz de invertir los elementos de una lista de listas.
-- > reverse'' [[1,2,3],[3,4,5]]
--[[5,4,3],[3,2,1]]
-- > reverse'' ["pepe", "casa", "patio"]
--["oitap","asac","epep"]
rever::[[a]]->[[a]]
rever=foldr(\n ns->ns ++[reve n])[]
--p) Implementar la funci�n predefinida de la librer�a est�ndar flip. 
-- Esta funci�n lo que hace es recibir una funci�n y
-- devolver otra funci�n que es id�ntica a la funci�n original, salvo que intercambia 
-- los dos primeros par�metros.
-- > flip' zip [1,2,3] "casa"
--[('c',1),('a',2),('s',3)]
-- > flip' (+) 3 4
--7
-- > flip' (++) "casa" "pollo"
--"pollocasa"
flip'::(a->b->c)->(b->a->c)
flip' f x y = f y x


--q) Implementar la funci�n polim�rfica predefinida de la librer�a 
-- est�ndar map. Esta funci�n lo que hace es recibir una funci�n 
-- y una lista y devuelve la lista resultante de aplicar la funci�n a cada
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
--1. Analiza cual ser�a el resultado de aplicar las siguientes funciones sobre la lista [2,3,5]
--a. foldr (\ a b-> a:b)[]  [2,3,5]

--b. foldr (\a b->[a]:b)[] [2,3,5]

--c. foldl (\ a b-> if b== 2 then 0:a else b:a)[]

--d. foldl (\ a b-> a:b)[]

--2. Desarrolla la ejecuci�n de la funci�n incognita para ver qu� har�a 
-- sobre la lista de n�meros enteros incluida en su invocaci�n
--incognita l = foldl (\ (y:z:xs) x -> if odd x then [y++[x],z]  else [y,z++[x]])[[],[]] [14,5,8,7,9,16]


--3. Desarrolla diferentes funciones que hagan uso de alguna de las versiones de la funci�n fold y que:
--a. Reciba una lista de enteros y devuelva la suma de sus dobles.
dobles:: [Int]->[Int]
dobles xs = foldl(\a b->  2*b:a)[] xs
--b. Reciba una lista de enteros y devuelva la suma de sus cuadrados.
cuadrado::[Int]->Int
cuadrado xs = sum(foldl(\a b-> b^2:a)[]xs)

cuadrado'::[Int]->Int
cuadrado' xs = sum(foldr(\a b-> a^2:b)[] xs)
--c. Reciba una lista de enteros y un entero y lo inserte al final de dicha lista.
finalL::[Int]->Int->[Int]
finalL xs x= foldl (\a b-> a++[b])[](xs++[x])

finalR::[Int]->Int->[Int]
finalR xs x = foldr (\a b-> a:b)[] (xs++[x])

--d. Reciba una lista y un n�mero entero y devuelva dicha lista eliminando las 
-- apariciones de ese n�mero entero
aparicionesL::[Int]->Int->[Int]
aparicionesL xs y=foldl(\a b -> if (b==y) then a else a++[b])[] xs

aparicionesR::[Int]->Int->[Int]
aparicionesR xs y=foldr(\a b-> if (a==y)then b else b++[a])[] xs

-- EJERCICIOS HOJA 3
--a) Se pide una funci�n que dada una lista de racionales, 
-- donde cada racional se define como dos n�meros enteros (numerador y denominador), 
-- y un n�mero racional, devuelva otra lista con todos los racionales 
-- equivalentes al dado. Realiza dos versiones del ejercicio:
--1. Empleando type.
--2. Empleando data.

--Ejemplos de aplicaci�n (si se utiliza type) ser�an:
-- > equivalentes [(2,4),(3,5),(4,8)] (1,2)
--[(2.0,4.0),(4.0,8.0)]
-- > equivalents [(3,5)] (1,2)
--[]
--Ejemplos de aplicaci�n (si se utiliza data) ser�an:
-- > equivalentes[R(2,4),R(3,5),R(4,8)] (R(1,2))
--[R (2.0,4.0),R (4.0,8.0)]
-- > equivalentes [R(3,5)] (R(1,2))

-- Tipo 1

type Numerador= Int
type Denominador= Int
type Racional=(Numerador,Denominador)

equivalentes::[Racional]->Racional->[Racional]
equivalentes = equivalentesAux []

equivalentesAux :: [Racional]->[Racional]-> Racional ->[Racional]
equivalentesAux r [] _= r
equivalentesAux' r (ra:ras) rae= equivalentesAux(if sonEquivalentes rae ra then r++[ra] else r)  ras rae

 
 
sonEquivalentes::Racional->Racional->Bool
sonEquivalentes(n1,d1)(n2,d2)=n1`rem`n2==0 && d1`rem`d2 ==0
 
 
 -- Tipo 2
data Rac=Ra{num::Integer,den::Integer}
sonEquivalentes'::Rac->Rac->Bool
sonEquivalentes'(Ra n1 d1)(Ra n2 d2)= n1 `rem`n2==0 && d1`rem`d2==0
-- forma 2
sonEquivalentes''::Rac->Rac->Bool
sonEquivalentes'' ra1 ra2=(num ra1)`rem`(num ra2)==0 && (den ra1)`rem`(den ra2)==0
 
--b) Se pide varias funciones para hacer lo siguiente:
--1. Funci�n que dado un punto de coordenadas y una direcci�n (Norte, Sur, Este u Oeste) 
--mueva el punto hacia la direcci�n indicada. Un ejemplo de aplicaci�n de la funci�n ser�a:
-- > mover Este (3,4) > mover Norte (3.5,9.2)
--(4,4) (3.5,10.2)
--version1 
data Direccion=Norte|Sur|Este|Oeste deriving Show
type PuntoCardinal1 = (Float,Float)
mover1::Direccion->PuntoCardinal1->PuntoCardinal1
mover1 Norte (x,y)=(x,y+1)
mover1 Sur (x,y)=(x+1,y)
mover1 Este (x,y)=(x,y-1)
mover1 Oeste (x,y)=(x-1,y)

--version2
data PuntoCardinal2=PC{x::Float,y::Float} deriving Show
mover2::Direccion->PuntoCardinal2->PuntoCardinal2
mover2 Norte (PC x y)= (PC x (y+1))
mover2 Sur   (PC x y)= (PC (x+1) y)
mover2 Este  (PC x y)= (PC (x-1) y)
mover2 Oeste (PC x y)= (PC x (y-1))


--2. Funci�n que dados dos puntos de coordenadas indique cu�l est� m�s al sur. 
-- Ejemplos de aplicaci�n de la funci�n son:
-- > masAlSur (3,5) (4,6) > masAlSur (4.5,-6.2) (4.5,-7)
--(3.0,5.0) (4.5,-7.0)

--version1
masAlSur::PuntoCardinal1->PuntoCardinal1->PuntoCardinal1
masAlSur (x1,y1)(x2,y2)= if (x1>x2) then (x1,y1) else (x2,y2)  

--version2

masAlSur':: PuntoCardinal2->PuntoCardinal2->PuntoCardinal2
masAlSur' pc1 pc2=if (y pc1)<(y pc2) then pc1 else pc2

--3. Funci�n que calcule la distancia entre dos puntos:
-- > distancia (3,5) (6,7)

-- version1
distancia1::PuntoCardinal1->PuntoCardinal1->Float
distancia1 (x1,y1)(x2,y2)=sqrt((x2-x1)^2 + (y2-y1)^2)


-- version2
distancia2::PuntoCardinal2->PuntoCardinal2->Float
distancia2 pc1 pc2= sqrt(((x pc2)-(x pc2))^2+((y pc2)-(y pc2))^2)



--4. Funci�n que dado un punto y una lista de direcciones, 
--retorne el camino que forman todos los puntos despu�s 
-- de cada movimiento sucesivo desde el punto original:
-- >camino (3.2,5.5) [Sur,Este,Este,Norte,Oeste]
--[(3.2,4.5),(4.2,4.5),(5.2,4.5),(5.2,5.5),(4.2,5.5)]


camino::PuntoCardinal2->[Direccion]->[PuntoCardinal2]
camino= caminoAux[]

caminoAux::[PuntoCardinal2]->PuntoCardinal2->[Direccion]->[PuntoCardinal2]
caminoAux r _ []= r
caminoAux r pc (d:ds)= caminoAux (r ++ [pf])pf ds
	where
		pf= mover2 d pc




--c) La empresa RealTimeSolutions, Inc. est� trabajando en un controlador para una central dom�tica.
--   El controlador recibe informaci�n de termostatos situados en diferentes habitaciones de la vivienda y bas�ndose en esta informaci�n, 
--   activa o desactiva el aire acondicionado en cada una de las habitaciones. Los termostatos pueden enviar la informaci�n sobre la temperatura 
--   en grados Celsius o Fahrenheit.
--   A su vez, los aparatos de aire acondicionado reciben dos tipos de �rdenes: apagar y encender (on y off). Se pide:

--1. Definir un tipo de datos para representar las temperaturas en ambos tipos de unidades.
  
type Celsius= Float;
type Faranheaid = Float;
data Temperatura = Celsius|Faranheaid deriving Show
data Temperatura'= Celsius' Float|Fahrenheit' Float deriving Show

     
--2. Definir una funci�n convert que dada una temperatura en grados Celsius la convierta a grados Fahrenheit y viceversa. 
--(Conversi�n de C a F: f = c * 9/5 + 32; conversi�n de F a C: c = (f � 32) * 5/9.)
-- Type
conversionCF::Celsius->Faranheaid
conversionCF f=   (f-32) * 5/9 

conversionFC::Faranheaid->Celsius
conversionFC c=c * (9 / 5) + 32

-- Data
convertCtoF'::Temperatura'->Temperatura'
convertCtoF'(Celsius' c)=(Fahrenheit' (c * (9 / 5) + 32))

convertFtoC'::Temperatura'->Temperatura'
convertFtoC'(Fahrenheit' f)= (Celsius'(f - 32 * (5 / 9)))

--3. Definir un tipo de datos para representar las �rdenes a los aparatos de a/a.

data OrdenaAA = On|Off deriving Show
--4. Definir una funci�n action que dada una temperatura en cierta habitaci�n determine la acci�n a realizar 
--sobre el aparato de a/a de dicha habitaci�n. El controlador debe encender el aparato si la temperatura excede de 28�C. 
-- Ejemplos de aplicaci�n:
-- > action(Celsius(25)) > action(Fahrenheit(83.5))
--On Off

action::Temperatura'->OrdenaAA
action  (Celsius' c)= if c>28 then  On else  Off
action  (Fahrenheit' f)=  action(convertFtoC' (Fahrenheit' f))

--d) Definir un tipo moneda para representar euros y d�lares USA. Definir una funci�n que convierta
-- entre ambas monedas sabiendo que el factor de conversi�n de euros a d�lares es 1.14.
-- conver (Dollar' 3.3)
type Dolar= Float
type Euro= Float

data Moneda = Dolar' Float| Euro' Float

-- type
conversion::Euro->Dolar
conversion e= 1.4*e

-- data
conversion'::Moneda->Moneda
conversion'(Euro' e)= (Dolar' (e*1.4))
 
--e) Dada el siguiente tipo de datos recursivo que representa expresiones aritm�ticas:
data Expr = Valor Integer
	|Expr :+: Expr
	|Expr :-: Expr
	|Expr :*: Expr deriving Show
 
--e.1) Se pide una funci�n para calcular el valor de una expresi�n.
calcularExp::Expr->Integer
calcularExp(Valor v) = v
calcularExp(expr1:+:expr2)=(calcularExp expr1) + (calcularExp expr2)
calcularExp(expr1:-:expr2)=(calcularExp expr1) - (calcularExp expr2)
calcularExp(expr1:*:expr2)=(calcularExp expr1) * (calcularExp expr2)


--e.2) Se pide una funci�n para calcular el n�mero de constantes de una expresi�n.
calcularConstante::Expr->Integer
calcularConstante(Valor v)=0
calcularConstante(expr1:+:expr2)=1+(calcularConstante expr1)+(calcularConstante expr2)
calcularConstantes(expr1:-:expr2)=1+(calcularConstante expr1)-(calcularConstante expr2)
calcularConstantes(expr1:*:expr2)=1+(calcularConstante expr1)*(calcularConstante expr2)

--f) Dado el siguiente tipo de datos que representa un �rbol binario:
--espejo (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
-- Rama (Rama AV 4 AV) 5 (Rama AV 8 (Rama AV 60 AV))


data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

espejo::Arbol a->Arbol a
espejo AV = AV
espejo (Rama AV r AV)=Rama AV r AV
espejo (Rama i r d)= (Rama ed r ei)
		where ei =espejo i ; ed= espejo d 


-- EJERCICIOS HOJA 4

--a) Definir una funci�n que dado un d�a de la semana, indique si �ste es o no laborable. 
-- Para representar el d�a de la semana se deber� crear un nuevo tipo enumerado.

data Dia= Lunes|Martes|Miercoles|Jueves|Viernes|Sabado deriving (Show,Eq)

laborable::Dia->Bool
laborable x= case x of
		 Lunes -> True 
		 Martes -> True 
		 Miercoles -> True 
		 Jueves -> True 
		 Viernes -> True 
		 otherwise -> False   

--b) Se quiere ordenar los elementos de una lista (cuyos elementos son comparables)
-- mediante el algoritmo del quicksort.
quickshort::(Ord a)=>[a]->[a]
quickshort[]=[]
quickshort (e:es)= quickshort ordenI ++[e]++ quickshort ordenD
	where 
		ordenI=[n|n<-es,n<e]
		ordenD=[n|n<-es,n>=e]

--c) Se pide implementar una funci�n que dada un n�mero (de cualquier tipo que soporte la operaci�n de divisi�n) 
--   y una lista de n�meros del mismo tipo, divida a ese n�mero por cada uno de los elementos contenidos 
--   en la lista y devuelva una lista con el resultado.
--Ejemplos de aplicaci�n de la funci�n son:
-- > divisiones 5 [1,2,3]
--[Just 5,Just 2,Just 1]
-- > divisiones 5 [1,2,3,0,9,10]
--[Just 5,Just 2,Just 1,Nothing,Just 0,Just 0]



--d) Dado un nuevo tipo de datos para representar un �rbol binario de cualquier tipo, definido como sigue:
--data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show
--Se pide definir una funci�n que visualice el �rbol por pantalla de una determinada forma: separando cada hijo izquierdo y derecho por �|�, la ra�z entre guiones y cada nivel diferente del �rbol por �( )�. Ejemplos de aplicaci�n de la funci�n ser�a los siguientes:
-- > mostrarArbol (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
--"((60)|-8-|())|-5-|(4)"
-- > mostrarArbol (Rama AV 5 (Rama AV 4 AV))
--"()|-5-|(4)"
--�Ser�a equivalente a declarar el nuevo tipo de datos Arbol como una instancia de la clase Show?


---e) Se quiere poder mostrar por pantalla los datos de los estudiantes matriculados en una universidad que pertenezcan
--  a alguna de las asociaciones de �sta (culturales, deportivas,
--de representaci�n estudiantil, etc.). Para ello se deber�n crear nuevos tipos de datos que representen:
--Estudiante, de cada uno se debe disponer del nombre y titulaci�n
 --Titulaci�n, que pueden ser tres: Grado II, Grado II_ADE, Grado ADE
--Lista de estudiantes matriculadosLista de estudiantes que pertenecen a asociaciones
--Un ejemplo de aplicaci�n de la funci�n que se pide podr�a ser:
-- > mostrarAlumnosAsociaciones(listaMatriculados,listaAsociaciones)
--"(Carlos Calle,GradoADE_II)(Irene Plaza,GradoADE)"
--Donde Carlos Calle e Irene Plaza son los �nicos estudiantes matriculados que pertenecen a alg�n tipo de asociaci�n en la universidad.
---- 

--f) Se quiere poder representar una fecha de la siguiente forma: dd/mm/aaaa, para ello se deber� crear 
-- un nuevo tipo de datos en Haskell. Por ejemplo, si se crea un nuevo tipo de datos cuyo constructor de datos es Fecha, en el int�rprete 
-- al poner fechas concretas nos devolver�a la representaci�n de la fecha que hayamos definido:
-- > Fecha 10 10 2013 > Fecha 24 12 2012
--10/10/2013 24/12/2012


--g) Teniendo en cuenta el nuevo tipo de datos Fecha definido anteriormente, se pide una funci�n 
-- que sea capaz de comparar dos fechas. Ejemplos de aplicaci�n de la funci�n ser�an:
-- > mismaFecha (Fecha 10 10 2013) (Fecha 10 10 2013)
--True
-- > mismaFecha (Fecha' 10 11 2013) (Fecha' 10 10 2013)
--False


--h) Teniendo en cuenta la definici�n de la funci�n qs del apartado (b) de este listado de ejercicios, se pide ordenar una lista de fechas mediante quicksort. Ejemplos de aplicaci�n de la funci�n ser�an:
-- > qs [(Fecha 10 10 2013), (Fecha 24 12 2012), (Fecha 10 09 2013), (Fecha 12 12 2013)]
--[24/12/2012,10/9/2013,10/10/2013,12/12/2013]

  
--instance Ord Fecha' where
 -- (<) (Fecha' d1 m1 a1) (Fecha' d2 m2 a2)
  --  | a1 < a2 = True
 --   | a1 == a2 && m1 < m2 = True
 --   | a1 == a2 && m1 == m2 && d1 < d2 = True
  --  | otherwise = False




--i) Se pide crear una nueva clase de tipos, llamada Coleccion, para representar colecciones de datos de cualquier tipo, donde los tipos pertenecientes a esta clase tendr�n el siguiente comportamiento:
--esVacia: funci�n para saber si la colecci�n est� vac�a.
--insertar: insertar� un nuevo elemento en la colecci�n.
--primero: devolver� el primer elemento de la colecci�n.
--eliminar: eliminar� un elemento de la colecci�n.
--size: devolver� el n�mero de elementos de la colecci�n.
--Algunas de las funciones anteriores variar�n su implementaci�n en funci�n del tipo de colecci�n particular que sea instancia de la clase Coleccion. Por ello, se pide crear dos instancias diferentes de esta clase para los dos nuevos tipos de datos que se presentan a continuaci�n:
--data Pila a = Pil [a] deriving Show
--data Cola a = Col [a] deriving Show
--El primero de ellos representa una estructura de datos LIFO con elementos de tipo a. El segundo representa una estructura de datos FIFO de elementos de tipo a.
--Ejemplos de aplicaci�n de las funciones para ambos tipos de datos ser�an:
-- > insertar 10 (Col [1,2,3,4])
--Col [1,2,3,4,10]
-- > insertar 10 (Pil [1,2,3,4])
--Pil [1,2,3,4,10]
-- > primero (Col [1,2,3,4,10])
--1
-- > primero (Pil [1,2,3,4,10])
--10
-- > eliminar (Col [1,2,3,4,10])
--Col [2,3,4,10]
-- > eliminar (Pil [1,2,3,4,10])
--Pil [1,2,3,4]




-- insertar 10 (Pil [1,2,3,4])





--GUION
--ejercicio4
-- if
contiene:: Int->Bool
contiene x=  if ((x<9)&&(x>0)) then True else False

-- case			
entre0y9 :: Integer -> Bool
entre0y9 x = case x of
	1 -> True
	2 -> True
	3 -> True
	4 -> True
	5 -> True
	6 -> True
	7 -> True
	8 -> True
	otherwise -> False

-- guards
contiene':: Int->Bool
contiene' x |(x > 0) && (x < 9) = True
		    | otherwise = False	
 
cuad:: Int->Int->Int
cuad x y  = (x^2 + y^2)

--BISIESTOS
 
divisible::(Int,Int)-> Bool
divisible(t,n) = t `rem` n == 0

bisiesto :: Int->Bool
bisiesto a = divisible(a,4) && (not(divisible(a,100)) || divisible(a,400))

meses ::Int->[Int]
meses a = [31,feb,31,30,31,30,31,31,30,31,30,31]
		where  feb |bisiesto a = 29
				   |otherwise = 28
 

-- ModuleName>insertarEn(7,[1,2,3,4],0)     [7,1,2,3,4]
insertarEn :: (Int, [Int],Int) -> [Int]
insertarEn (x, lista, n) = let (ys,zs) = (take n lista, drop n lista) in ys++x:zs

--suma
sumi::[Int]->Int
sumi[]=0
sumi(x:xs)=x+sumi xs


contarDigitos x = contarDigitosRecCola(x,1)
contarDigitosRecCola :: (Integer,Integer) -> Integer
contarDigitosRecCola(x,c) = if x `div`10 == 0 then c
							else contarDigitosRecCola(x `div`10,c+1)


--Problemas
--mezclarEnTernas::([a],[b])->[(a,b,b)]
--mezclarEnTernas([],_) = []
--mezclarEnTernas(_,[]) = []
--mezclarEnTernas(_,x:[]) = [] -- Un �nico elemento no vale
--mezclarEnTernas(c1:r1,c2:c3:r2) = (c1,c2,c3): mezclarEnTernas(r1,r2)



