
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

--2. Desarrolla la ejecución de la función incognita para ver qué haría 
-- sobre la lista de números enteros incluida en su invocación
--incognita l = foldl (\ (y:z:xs) x -> if odd x then [y++[x],z]  else [y,z++[x]])[[],[]] [14,5,8,7,9,16]


--3. Desarrolla diferentes funciones que hagan uso de alguna de las versiones de la función fold y que:
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

--d. Reciba una lista y un número entero y devuelva dicha lista eliminando las 
-- apariciones de ese número entero
aparicionesL::[Int]->Int->[Int]
aparicionesL xs y=foldl(\a b -> if (b==y) then a else a++[b])[] xs

aparicionesR::[Int]->Int->[Int]
aparicionesR xs y=foldr(\a b-> if (a==y)then b else b++[a])[] xs

-- EJERCICIOS HOJA 3
--a) Se pide una función que dada una lista de racionales, 
-- donde cada racional se define como dos números enteros (numerador y denominador), 
-- y un número racional, devuelva otra lista con todos los racionales 
-- equivalentes al dado. Realiza dos versiones del ejercicio:
--1. Empleando type.
--2. Empleando data.

--Ejemplos de aplicación (si se utiliza type) serían:
-- > equivalentes [(2,4),(3,5),(4,8)] (1,2)
--[(2.0,4.0),(4.0,8.0)]
-- > equivalents [(3,5)] (1,2)
--[]
--Ejemplos de aplicación (si se utiliza data) serían:
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
--1. Función que dado un punto de coordenadas y una dirección (Norte, Sur, Este u Oeste) 
--mueva el punto hacia la dirección indicada. Un ejemplo de aplicación de la función sería:
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


--2. Función que dados dos puntos de coordenadas indique cuál está más al sur. 
-- Ejemplos de aplicación de la función son:
-- > masAlSur (3,5) (4,6) > masAlSur (4.5,-6.2) (4.5,-7)
--(3.0,5.0) (4.5,-7.0)

--version1
masAlSur::PuntoCardinal1->PuntoCardinal1->PuntoCardinal1
masAlSur (x1,y1)(x2,y2)= if (x1>x2) then (x1,y1) else (x2,y2)  

--version2

masAlSur':: PuntoCardinal2->PuntoCardinal2->PuntoCardinal2
masAlSur' pc1 pc2=if (y pc1)<(y pc2) then pc1 else pc2

--3. Función que calcule la distancia entre dos puntos:
-- > distancia (3,5) (6,7)

-- version1
distancia1::PuntoCardinal1->PuntoCardinal1->Float
distancia1 (x1,y1)(x2,y2)=sqrt((x2-x1)^2 + (y2-y1)^2)


-- version2
distancia2::PuntoCardinal2->PuntoCardinal2->Float
distancia2 pc1 pc2= sqrt(((x pc2)-(x pc2))^2+((y pc2)-(y pc2))^2)



--4. Función que dado un punto y una lista de direcciones, 
--retorne el camino que forman todos los puntos después 
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




--c) La empresa RealTimeSolutions, Inc. está trabajando en un controlador para una central domótica.
--   El controlador recibe información de termostatos situados en diferentes habitaciones de la vivienda y basándose en esta información, 
--   activa o desactiva el aire acondicionado en cada una de las habitaciones. Los termostatos pueden enviar la información sobre la temperatura 
--   en grados Celsius o Fahrenheit.
--   A su vez, los aparatos de aire acondicionado reciben dos tipos de órdenes: apagar y encender (on y off). Se pide:

--1. Definir un tipo de datos para representar las temperaturas en ambos tipos de unidades.
  
type Celsius= Float;
type Faranheaid = Float;
data Temperatura = Celsius|Faranheaid deriving Show
data Temperatura'= Celsius' Float|Fahrenheit' Float deriving Show

     
--2. Definir una función convert que dada una temperatura en grados Celsius la convierta a grados Fahrenheit y viceversa. 
--(Conversión de C a F: f = c * 9/5 + 32; conversión de F a C: c = (f – 32) * 5/9.)
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

--3. Definir un tipo de datos para representar las órdenes a los aparatos de a/a.

data OrdenaAA = On|Off deriving Show
--4. Definir una función action que dada una temperatura en cierta habitación determine la acción a realizar 
--sobre el aparato de a/a de dicha habitación. El controlador debe encender el aparato si la temperatura excede de 28ºC. 
-- Ejemplos de aplicación:
-- > action(Celsius(25)) > action(Fahrenheit(83.5))
--On Off

action::Temperatura'->OrdenaAA
action  (Celsius' c)= if c>28 then  On else  Off
action  (Fahrenheit' f)=  action(convertFtoC' (Fahrenheit' f))

--d) Definir un tipo moneda para representar euros y dólares USA. Definir una función que convierta
-- entre ambas monedas sabiendo que el factor de conversión de euros a dólares es 1.14.
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
 
--e) Dada el siguiente tipo de datos recursivo que representa expresiones aritméticas:
data Expr = Valor Integer
	|Expr :+: Expr
	|Expr :-: Expr
	|Expr :*: Expr deriving Show
 
--e.1) Se pide una función para calcular el valor de una expresión.
calcularExp::Expr->Integer
calcularExp(Valor v) = v
calcularExp(expr1:+:expr2)=(calcularExp expr1) + (calcularExp expr2)
calcularExp(expr1:-:expr2)=(calcularExp expr1) - (calcularExp expr2)
calcularExp(expr1:*:expr2)=(calcularExp expr1) * (calcularExp expr2)


--e.2) Se pide una función para calcular el número de constantes de una expresión.
calcularConstante::Expr->Integer
calcularConstante(Valor v)=0
calcularConstante(expr1:+:expr2)=1+(calcularConstante expr1)+(calcularConstante expr2)
calcularConstantes(expr1:-:expr2)=1+(calcularConstante expr1)-(calcularConstante expr2)
calcularConstantes(expr1:*:expr2)=1+(calcularConstante expr1)*(calcularConstante expr2)

--f) Dado el siguiente tipo de datos que representa un árbol binario:
--espejo (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
-- Rama (Rama AV 4 AV) 5 (Rama AV 8 (Rama AV 60 AV))


data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

espejo::Arbol a->Arbol a
espejo AV = AV
espejo (Rama AV r AV)=Rama AV r AV
espejo (Rama i r d)= (Rama ed r ei)
		where ei =espejo i ; ed= espejo d 


-- EJERCICIOS HOJA 4

--a) Definir una función que dado un día de la semana, indique si éste es o no laborable. 
-- Para representar el día de la semana se deberá crear un nuevo tipo enumerado.

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

--c) Se pide implementar una función que dada un número (de cualquier tipo que soporte la operación de división) 
--   y una lista de números del mismo tipo, divida a ese número por cada uno de los elementos contenidos 
--   en la lista y devuelva una lista con el resultado.
--Ejemplos de aplicación de la función son:
-- > divisiones 5 [1,2,3]
--[Just 5,Just 2,Just 1]
-- > divisiones 5 [1,2,3,0,9,10]
--[Just 5,Just 2,Just 1,Nothing,Just 0,Just 0]



--d) Dado un nuevo tipo de datos para representar un árbol binario de cualquier tipo, definido como sigue:
--data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show
--Se pide definir una función que visualice el árbol por pantalla de una determinada forma: separando cada hijo izquierdo y derecho por “|”, la raíz entre guiones y cada nivel diferente del árbol por “( )”. Ejemplos de aplicación de la función sería los siguientes:
-- > mostrarArbol (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
--"((60)|-8-|())|-5-|(4)"
-- > mostrarArbol (Rama AV 5 (Rama AV 4 AV))
--"()|-5-|(4)"
--¿Sería equivalente a declarar el nuevo tipo de datos Arbol como una instancia de la clase Show?


---e) Se quiere poder mostrar por pantalla los datos de los estudiantes matriculados en una universidad que pertenezcan
--  a alguna de las asociaciones de ésta (culturales, deportivas,
--de representación estudiantil, etc.). Para ello se deberán crear nuevos tipos de datos que representen:
--Estudiante, de cada uno se debe disponer del nombre y titulación
 --Titulación, que pueden ser tres: Grado II, Grado II_ADE, Grado ADE
--Lista de estudiantes matriculadosLista de estudiantes que pertenecen a asociaciones
--Un ejemplo de aplicación de la función que se pide podría ser:
-- > mostrarAlumnosAsociaciones(listaMatriculados,listaAsociaciones)
--"(Carlos Calle,GradoADE_II)(Irene Plaza,GradoADE)"
--Donde Carlos Calle e Irene Plaza son los únicos estudiantes matriculados que pertenecen a algún tipo de asociación en la universidad.
---- 

--f) Se quiere poder representar una fecha de la siguiente forma: dd/mm/aaaa, para ello se deberá crear 
-- un nuevo tipo de datos en Haskell. Por ejemplo, si se crea un nuevo tipo de datos cuyo constructor de datos es Fecha, en el intérprete 
-- al poner fechas concretas nos devolvería la representación de la fecha que hayamos definido:
-- > Fecha 10 10 2013 > Fecha 24 12 2012
--10/10/2013 24/12/2012


--g) Teniendo en cuenta el nuevo tipo de datos Fecha definido anteriormente, se pide una función 
-- que sea capaz de comparar dos fechas. Ejemplos de aplicación de la función serían:
-- > mismaFecha (Fecha 10 10 2013) (Fecha 10 10 2013)
--True
-- > mismaFecha (Fecha' 10 11 2013) (Fecha' 10 10 2013)
--False


--h) Teniendo en cuenta la definición de la función qs del apartado (b) de este listado de ejercicios, se pide ordenar una lista de fechas mediante quicksort. Ejemplos de aplicación de la función serían:
-- > qs [(Fecha 10 10 2013), (Fecha 24 12 2012), (Fecha 10 09 2013), (Fecha 12 12 2013)]
--[24/12/2012,10/9/2013,10/10/2013,12/12/2013]

  
--instance Ord Fecha' where
 -- (<) (Fecha' d1 m1 a1) (Fecha' d2 m2 a2)
  --  | a1 < a2 = True
 --   | a1 == a2 && m1 < m2 = True
 --   | a1 == a2 && m1 == m2 && d1 < d2 = True
  --  | otherwise = False




--i) Se pide crear una nueva clase de tipos, llamada Coleccion, para representar colecciones de datos de cualquier tipo, donde los tipos pertenecientes a esta clase tendrán el siguiente comportamiento:
--esVacia: función para saber si la colección está vacía.
--insertar: insertará un nuevo elemento en la colección.
--primero: devolverá el primer elemento de la colección.
--eliminar: eliminará un elemento de la colección.
--size: devolverá el número de elementos de la colección.
--Algunas de las funciones anteriores variarán su implementación en función del tipo de colección particular que sea instancia de la clase Coleccion. Por ello, se pide crear dos instancias diferentes de esta clase para los dos nuevos tipos de datos que se presentan a continuación:
--data Pila a = Pil [a] deriving Show
--data Cola a = Col [a] deriving Show
--El primero de ellos representa una estructura de datos LIFO con elementos de tipo a. El segundo representa una estructura de datos FIFO de elementos de tipo a.
--Ejemplos de aplicación de las funciones para ambos tipos de datos serían:
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
--mezclarEnTernas(_,x:[]) = [] -- Un único elemento no vale
--mezclarEnTernas(c1:r1,c2:c3:r2) = (c1,c2,c3): mezclarEnTernas(r1,r2)



