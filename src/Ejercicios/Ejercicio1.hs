
module Ejercicios.Ejercicio1 where

import Data.Char
--Reload 
-- Hoja 1 online
-- a) Diseñar una función en Haskell que dados tres elementos enteros determine si están
-- ordenados de menor a mayor.

mayorMenor::Int->Int->Int->Bool
mayorMenor a b c = (a>=b)&&(b>=c) 




-- b) Diseñar una función en Haskell que dados tres elementos enteros los ordene de menor
-- a mayor.
-- if 
ordena1::(Int,Int,Int)->(Int,Int,Int)
ordena1 (a,b,c) =  if (a>=b)&&(a>=c)&&(b>=c) then (a,b,c) else
				if (a>=b)&&(a>=c)&&(c>=b) then (a,c,b) else
				if (b>=a)&&(b>=c)&&(a>=c) then (b,a,c) else
				if (b>=a)&&(b>=c)&&(c>=a) then (b,c,a) else
				if (c>=a)&&(c>=b)&&(a>=b) then (c,a,b) else
				 (c,b,a) 
				
			  				


-- Ejercicio (b)
-- Ordena tres elementos

ordena::(Int,Int,Int)->(Int,Int,Int)
ordena(x,y,z)
			|estanOrd(x,y,z) = (x,y,z)
			|x<=y = if z<=x then (z,x,y) else(x,z,y)
			|z>=x = (y,x,z)
			|otherwise = if z>=y then (y,z,x) else (z,y,x)

estanOrd::(Int,Int,Int)->Bool
estanOrd(x,y,z)= (x<=y)&&(y<=z)				
				

--c) Implementar en Haskell una función que reciba un número real y devuelva una tupla
--con su parte entera y sus dos primeros decimales (como número entero).
convierte::Float->(Int,Int)
convierte x= (truncate(x),truncate(x*100)-100*truncate(x))


--d) Diseñar una función que reciba una 2-tupla de números enteros y devuelva el cociente y
--el resto de la división entera, junto con un mensaje de que la operación se ha realizado
--de manera satisfactoria. En el caso de que la operación no se pudiera realizar, ha de
--devolver sendos ceros (como cociente y como resto) y un mensaje que discierna entre si
--el resultado es “infinito” (n div 0) o “indeterminación” (0 div 0). Implementar tres
--versiones distintas de la función, utilizando las tres estructuras condicionales vistas en
--clase.
 
divide::(Int,Int)->(String, Int,Int)
divide 	(x,y)= case (x,y) of
		(0,0) ->("indeterminacion",0,0)
		(_,0) ->("infinito",0,0)
		otherwise -> ("correcto",x`div`y,x`mod`y)



--e) Crear una función que reciba el radio de una circunferencia y devuelva una 2-tupla con
--la longitud de la circunferencia y con el área del círculo. Emplea una definición local con
--la cláusula where para almacenar el valor de Pi (Nota: no se debe utilizar la función
--predefinida pi). A continuación crear una función con el mismo cometido empleando la
--definición local let.

-- where
circulo ::Float->(Float,Float)
circulo x =(2*p * x,4*p*x^2)
		where 
			p=3.14

--let
circulo' ::Float->(Float,Float)
circulo' x = let p=3.14 in(2*p * x,4*p*x^2)
		



--f) Se pide diseñar una función que reciba como parámetros una lista de elementos enteros
--y un entero y añada el nuevo elemento al final de la misma.

add::[Int]->Int->[Int]
add xs y=  xs++[y]

--g) Dada una lista de elementos y una posición, implementar una función que devuelva el
--elemento que ocupa dicha posición dentro de la lista, sin emplear el operador de
--indirección !!. Considérese que la primera posición de la lista es la posición 0.

pos::[Int]->Int->Int
pos xs y=  xs !!y 



--h) Escribe un programa que reciba un número entero mayor que 2 y que devuelva, en caso  ***PROBAR PENSAR
--de que exista, la suma de dos números primos cuyo resultado sea el propio número. En
--caso de que no exista dicha pareja de números primos, puede devolver el propio
--número con el 0.
--sumaPrimos 12 = (1,11) sumaPrimos 17 = (17,0)
sumaPrimos::Int->(Int,Int)
sumaPrimos n= sumaprimosAux(n,1)

sumaprimosAux::(Int,Int)->(Int,Int)
sumaprimosAux (n,i)= if i >n `div` 2 then (n,0)
					else if (esPrimo i) && (esPrimo (n-i)) then (i,n-i)
						else sumaprimosAux (n,i+1)

esPrimo::Int->Bool
esPrimo n
		| n<=3 =True
		| n`mod`2 == 0 =False
		| otherwise = comprobarPrimacidad(n,3)
		

comprobarPrimacidad::(Int,Int)->Bool
comprobarPrimacidad(n,i)
					|i*i>n =True
					|n`mod`i == 0 = False
					|otherwise =  comprobarPrimacidad(n,i+2)
					

--Hoja 1 presencial

--Resuelve los siguientes ejercicios en Haskell (correspondientes a los temas 2 y 3 del temario de la asignatura).
--Ejercicios – Primera parte
--a) Implementar una función en Haskell que dados tres números enteros determine si están ordenados de menor a mayor.
ordenM::Int->Int->Int->Bool
ordenM a b c  = if((c>=b) && (c>=a))then  True else False  

--b) Implementar una función en Haskell que dados tres números enteros los devuelva ordenados de menor a mayor.


ordenar2::Int->Int->Int->(Int,Int,Int)
ordenar2 a b c  |ordenM a b c =(a,b,c)
				|((a<=c)&&(c<=b))=(b,c,a)
				|((c<=b)&&(a<=c))=(b,c,a)
				|otherwise =(0,0,0)
			

--c) Implementar en Haskell una función que reciba un número real y devuelva una tupla con su parte entera y sus dos primeros decimales (como número entero).

convierte2::Float->(Int,Int)
convierte2 x= (truncate(x),truncate(x*100)-100*truncate(x))
--d) Crear una función que reciba el radio de una circunferencia y devuelva una 2-tupla con la longitud de la circunferencia y con el área del círculo. Emplea una definición local con la cláusula where para almacenar el valor de Pi (Nota: no se debe utilizar la función predefinida pi). A continuación crear una función con el mismo cometido empleando la definición local let.
circulo2 ::Float->(Float,Float)
circulo2 x =(2*p * x,4*p*x^2)
		where 
			p=3.14

--let
circulo2' ::Float->(Float,Float)
circulo2' x = let p=3.14 in(2*p * x,4*p*x^2)
		
--e) Implementar la función predefinida de listas concat, que se llamará concatenar, 
-- utilizando la definición de listas por comprensión (no se puede utilizar recursividad).
concatenar::[[a]]->[a]
concatenar bss = [b|bs<-bss, b<-bs]


--f) Implementar una función que dado un número entero devuelva en una lista 
-- todos los factores de dicho número. Se debe utilizar la definición de listas por comprensión.
--En matemáticas, los factores de un número son los números enteros que pueden multiplicarse juntos para igualar ese número. 
--O también se puede decir que los factores de un número son números enteros por el que un número es divisible.

divisibles::Int->[Int]
divisibles y=[x | x <- [1..y], y`mod`x ==0]

--g) Implementar una función que diga si un número es primo. Para ello se debe utilizar la función que calcula el número de factores
--  de un número (ejercicio f).
--Nota: Si para resolver el ejercicio se deben comparar dos listas, se puede hacer con el operador de igualdad de listas (==). Por ejemplo:
-- > [1,2,3] == [1,2,3]
--True
-- > [1,2,3] == [1,2]
--False

primos::Int->Bool
primos x = [1,x] == divisibles x 

--h) Implementar una función que diga cuántos caracteres en mayúscula están contenidos en una frase dada. 
-- Se deberá utilizar la definición de listas por comprensión.

cuentaM::String->Int
cuentaM xs = length [x |x <- xs , isUpper x]
 

--i) Implementar una función que dada una tupla de tres elementos, donde cada uno de ellos es a su vez una tupla de dos
-- elementos de tipo String e Int respectivamente, retorne el primer elemento de cada tupla interna.
--  Se deberá utilizar ajuste de patrones.

primeraParte::((String,Int),(String,Int),(String,Int))->(String,String,String)
primeraParte((s1,r1),(s2,r2),(s3,r3))=(s1,s2,s3)


--j) Implementar una función que devuelve True si la suma de los cuatro primeros elementos de una lista de números enteros
--  es un valor menor a 10 y devolverá False en caso contrario. Se deberá utilizar ajuste de patrones.
sumaMen::[Int]->Bool
sumaMen [] = True
sumaMen [x1] = x1 < 10
sumaMen [x1,x2] = x1 < 10
sumaMen [x1,x2,x3] = x1 < 10
sumaMen (x1:x2:x3:x4:xs) =sum [x1,x2,x3,x4]<10 



--k) Implementar una función que dado un carácter, que representa un punto cardinal, devuelva su descripción.
--  Por ejemplo, dado ‘N’ devuelva “Norte”.

cardinales::Char->String
cardinales 'N'="Norte"
cardinales 'S'="Sur"
cardinales 'E'="Este"
cardinales 'O'="Oeste"
cardinales _ = "No existe"

--l) Implementar una función que dada una frase retorne un mensaje donde se indique cuál es la primera y última letra de la 
--frase original. Un ejemplo de aplicación de la función podría ser:
-- > procesarFrase "El perro de San Roque"
--"La primera letra de la frase ''El perro de San Roque'' es 'E' y la ultima letra es 'e'"
--Nota: No se permite el uso de recursividad. Se debe usar ajuste de patrones y se puede utilizar también 
--patrones nombrados (para referirse a la cadena de entrada).

primeraUltima::String->String
primeraUltima []= ""
primeraUltima [c]= "solo un elemento"
primeraUltima (x:xs)= [x]++ drop ((length xs)-1) xs 



--m) Implementar una función que dado un número entero devuelva mensajes indicando en qué rango de valores
-- se encuentra dicho número (menor de 10, entre 10 y 20 o mayor de 20). Se debe utilizar definiciones locales.
--Ejemplos de aplicación de la función son:
-- > clasificarValorEntrada 20
--"El valor de entrada es mayor o igual a 10 y menor o igual a 20"
-- > clasificarValorEntrada 9
--"El valor de entrada es menor que 10"
-- > clasificarValorEntrada 35
--"El valor de entrada es mayor que 20"

--Pista: La cadena “El valor de entrada” se repite constantemente, por ello una definición local tiene sentido para 
-- que sólo se defina una vez.


clasificarValorEntrada::Int->String
clasificarValorEntrada x  	
						| x<10 = headMessage ++"menor que"++show ten
						| x>=10 && x<=20 = headMessage++"entre"++ show ten ++"y"++ show tweenty
						| otherwise = headMessage++show ten ++"y"++show tweenty							
						where 
						headMessage = "valor mayor"
						ten = 10
						tweenty = 20




-- *9****************************************
--n) Implementar una función que dada una cadena de caracteres y un carácter, indique el número de apariciones del carácter en la cadena.
--  No se debe utilizar recursividad, sí ajuste de patrones. Pista: utilizar la definición de listas por comprensión.
--Ejemplos de aplicación de la función:
-- > contarApariciones "casa" 'c'
--1
-- > contarApariciones "casa" 'a'
--2
-- > contarApariciones "" 'c'
-- 0

contarApariciones::String->Char->Int
contarApariciones [] _= 0
contarApariciones xs y=length[x|x<-xs,x == y]


