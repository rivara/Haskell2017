
module Ejercicios.Ejercicio1 where
--Reload 
-- Hoja 1 online
-- a) Dise�ar una funci�n en Haskell que dados tres elementos enteros determine si est�n
-- ordenados de menor a mayor.

mayorMenor::Int->Int->Int->Bool
mayorMenor a b c = (a>=b)&&(b>=c) 




-- b) Dise�ar una funci�n en Haskell que dados tres elementos enteros los ordene de menor
-- a mayor.
-- if 
ordena1::(Int,Int,Int)->(Int,Int,Int)
ordena1 (a,b,c) =  if (a>=b)&&(a>=c)&&(b>=c) then (a,b,c) else
				if (a>=b)&&(a>=c)&&(c>=b) then (a,c,b) else
				if (b>=a)&&(b>=c)&&(a>=c) then (b,a,c) else
				if (b>=a)&&(b>=c)&&(c>=a) then (b,c,a) else
				if (c>=a)&&(c>=b)&&(a>=b) then (c,a,b) else
				 (c,b,a) 
				
			  
				
-- case

				
				
				
				

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
				

--c) Implementar en Haskell una funci�n que reciba un n�mero real y devuelva una tupla
--con su parte entera y sus dos primeros decimales (como n�mero entero).
convierte::Float->(Int,Int)
convierte x= (truncate(x),truncate(x*100)-100*truncate(x))


--d) Dise�ar una funci�n que reciba una 2-tupla de n�meros enteros y devuelva el cociente y
--el resto de la divisi�n entera, junto con un mensaje de que la operaci�n se ha realizado
--de manera satisfactoria. En el caso de que la operaci�n no se pudiera realizar, ha de
--devolver sendos ceros (como cociente y como resto) y un mensaje que discierna entre si
--el resultado es �infinito� (n div 0) o �indeterminaci�n� (0 div 0). Implementar tres
--versiones distintas de la funci�n, utilizando las tres estructuras condicionales vistas en
--clase.
 
divide::(Int,Int)->(String, Int,Int)
divide 	(x,y)= case (x,y) of
		(0,0) ->("indeterminacion",0,0)
		(_,0) ->("infinito",0,0)
		otherwise -> ("correcto",x`div`y,x`mod`y)



--e) Crear una funci�n que reciba el radio de una circunferencia y devuelva una 2-tupla con
--la longitud de la circunferencia y con el �rea del c�rculo. Emplea una definici�n local con
--la cl�usula where para almacenar el valor de Pi (Nota: no se debe utilizar la funci�n
--predefinida pi). A continuaci�n crear una funci�n con el mismo cometido empleando la
--definici�n local let.

-- where
circulo ::Float->(Float,Float)
circulo x =(2*p * x,4*p*x^2)
		where 
			p=3.14

--let
circulo' ::Float->(Float,Float)
circulo' x = let p=3.14 in(2*p * x,4*p*x^2)
		



--f) Se pide dise�ar una funci�n que reciba como par�metros una lista de elementos enteros
--y un entero y a�ada el nuevo elemento al final de la misma.

add::[Int]->Int->[Int]
add xs y=  xs++[y]

--g) Dada una lista de elementos y una posici�n, implementar una funci�n que devuelva el
--elemento que ocupa dicha posici�n dentro de la lista, sin emplear el operador de
--indirecci�n !!. Consid�rese que la primera posici�n de la lista es la posici�n 0.

pos::[Int]->Int->Int
pos xs y=  xs !!y 



--h) Escribe un programa que reciba un n�mero entero mayor que 2 y que devuelva, en caso  ***PROBAR PENSAR
--de que exista, la suma de dos n�meros primos cuyo resultado sea el propio n�mero. En
--caso de que no exista dicha pareja de n�meros primos, puede devolver el propio
--n�mero con el 0.
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
--Ejercicios � Primera parte
--a) Implementar una funci�n en Haskell que dados tres n�meros enteros determine si est�n ordenados de menor a mayor.

--b) Implementar una funci�n en Haskell que dados tres n�meros enteros los devuelva ordenados de menor a mayor.

--c) Implementar en Haskell una funci�n que reciba un n�mero real y devuelva una tupla con su parte entera y sus dos primeros decimales (como n�mero entero).

--d) Crear una funci�n que reciba el radio de una circunferencia y devuelva una 2-tupla con la longitud de la circunferencia y con el �rea del c�rculo. Emplea una definici�n local con la cl�usula where para almacenar el valor de Pi (Nota: no se debe utilizar la funci�n predefinida pi). A continuaci�n crear una funci�n con el mismo cometido empleando la definici�n local let.

--e) Implementar la funci�n predefinida de listas concat, que se llamar� concatenar, utilizando la definici�n de listas por comprensi�n (no se puede utilizar recursividad).

--f) Implementar una funci�n que dado un n�mero entero devuelva en una lista todos los factores de dicho n�mero. Se debe utilizar la definici�n de listas por comprensi�n.
--En matem�ticas, los factores de un n�mero son los n�meros enteros que pueden multiplicarse juntos para igualar ese n�mero. O tambi�n se puede decir que los factores de un n�mero son n�meros enteros por el que un n�mero es divisible.

--g) Implementar una funci�n que diga si un n�mero es primo. Para ello se debe utilizar la funci�n que calcula el n�mero de factores de un n�mero (ejercicio f).
--Nota: Si para resolver el ejercicio se deben comparar dos listas, se puede hacer con el operador de igualdad de listas (==). Por ejemplo:
-- > [1,2,3] == [1,2,3]
--True
-- > [1,2,3] == [1,2]
--False

--h) Implementar una funci�n que diga cu�ntos caracteres en may�scula est�n contenidos en una frase dada. Se deber� utilizar la definici�n de listas por comprensi�n.
--Ejercicios � Segunda parte

--i) Implementar una funci�n que dada una tupla de tres elementos, donde cada uno de ellos es a su vez una tupla de dos elementos de tipo String e Int respectivamente, retorne el primer elemento de cada tupla interna. Se deber� utilizar ajuste de patrones.

--j) Implementar una funci�n que devuelve True si la suma de los cuatro primeros elementos de una lista de n�meros enteros es un valor menor a 10 y devolver� False en caso contrario. Se deber� utilizar ajuste de patrones.

--k) Implementar una funci�n que dado un car�cter, que representa un punto cardinal, devuelva su descripci�n. Por ejemplo, dado �N� devuelva �Norte�.

--l) Implementar una funci�n que dada una frase retorne un mensaje donde se indique cu�l es la primera y �ltima letra de la frase original. Un ejemplo de aplicaci�n de la funci�n podr�a ser:
-- > procesarFrase "El perro de San Roque"
--"La primera letra de la frase ''El perro de San Roque'' es 'E' y la ultima letra es 'e'"
--Nota: No se permite el uso de recursividad. Se debe usar ajuste de patrones y se puede utilizar tambi�n patrones nombrados (para referirse a la cadena de entrada).

--m) Implementar una funci�n que dado un n�mero entero devuelva mensajes indicando en qu� rango de valores se encuentra dicho n�mero (menor de 10, entre 10 y 20 o mayor de 20). Se debe utilizar definiciones locales.
--Ejemplos de aplicaci�n de la funci�n son:
-- > clasificarValorEntrada 20
--"El valor de entrada es mayor o igual a 10 y menor o igual a 20"
-- > clasificarValorEntrada 9
--"El valor de entrada es menor que 10"
-- > clasificarValorEntrada 35
--"El valor de entrada es mayor que 20"

--Pista: La cadena �El valor de entrada� se repite constantemente, por ello una definici�n local tiene sentido para que s�lo se defina una vez.
--n) Implementar una funci�n que dada una cadena de caracteres y un car�cter, indique el n�mero de apariciones del car�cter en la cadena. No se debe utilizar recursividad, s� ajuste de patrones. Pista: utilizar la definici�n de listas por comprensi�n.
--Ejemplos de aplicaci�n de la funci�n:
-- > contarApariciones "casa" 'c'
--1
-- > contarApariciones "casa" 'a'
--2
-- > contarApariciones "" 'c'
-- 0