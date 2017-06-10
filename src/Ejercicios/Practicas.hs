
module Ejercicios.Practicas where

--Pr�ctica 1
--El alumno debe conocer y dominar los aspectos b�sicos del lenguaje de programaci�n Haskell, as� como tener un conocimiento elemental del entorno de programaci�n EclipseGavab.
--Ejercicios pr�cticos:

--1. Se pide implementar en Haskell una funci�n que calcule la suma de los n primeros t�rminos de la siguiente serie: 1/1 � � + 1/3 � � � -1/2i + 1/(2i+1). Se recomienda utilizar dos funciones: pares(i) e impares(i); una que calcule la suma de los t�rminos pares y otra que calcule la suma de los t�rminos impares hasta un t�rmino concreto.

--2. Dise�a una funci�n recursiva que reciba un n�mero entero positivo e intercambie las posiciones de los d�gitos del n�mero situados en esas posiciones (se puede suponer que las unidades ocupan la posici�n 0).
--intercambiar (72549,0,3) = 79542 intercambiar (72549,1,2) = 72459

--3. Dise�a una funci�n capaz de determinar si un n�mero entero positivo es capic�a.

--4. Utilizando la funci�n del ejercicio anterior, dise�a una funci�n que reciba un n�mero entero positivo y busque su familiar capic�a. Dicho familiar se encuentra sumando al n�mero su inverso. Si el n�mero resultante es capic�a el proceso parar�, si no, se repetir� el proceso a partir del n�mero obtenido de la suma anterior.
--famCapicua (59) => famCapicua (154) => famCapicua (605) => famCapicua (1111) => 1111
--59 + 95
--154 + 451
--605 + 506
--Pr�ctica 1




--5. Escribe un programa que reciba un n�mero entero mayor que 2 y que devuelva, en caso de que exista, la suma de dos n�meros primos cuyo resultado sea el propio n�mero. En caso de que no exista dicha pareja de n�meros primos, puede devolver el propio n�mero con el 0.
--sumaPrimos (12) = (1,11) sumaPrimos(17)=(17,0)

--6. Se pide implementar en Haskell una funci�n que dada una lista de enteros, devuelva las sublistas entre cada par de ceros. Se recomienda hacer uso del operador de concatenaci�n (++), y de las funciones head y tail. Para que la lista resultante se devuelva en el mismo orden en el que aparec�an las sublistas dentro de la lista original se puede utilizar la funci�n reverse. Por ejemplo:
--parteLista([1,4,6,3,0,5,6,0,9]) = [[1,4,6,3], [5,6], [9]]
--parteLista([0,3,4,5]) = [[], [3,4,5]]
--parteLista([2,3]) = [[2,3]]
--parteLista([]) = []

--7. Dise�a una funci�n en Haskell que reciba una lista y devuelva las listas productos de la secuencia de inserciones de los elementos en cada lista anterior (empezando por la lista vac�a).
--segmentos [1,8,4,7] = [[],[1],[1,8],[1,8,4],[1,8,4,7]]

--8. Dada una lista de enteros, implementa una funci�n que ordene dicha lista de menor a mayor utilizando un algoritmo de inserci�n. Dicho algoritmo consiste en recorrer la lista L, insertando cada elemento L[i] en el lugar correcto entre los elementos ya ordenados L[1] ,...,L[i-1].
--insercion [8,-4,0,8,-9,2,1,94]= [-9,-4,0,1,2,8,8,94]

--9. Dada una lista de enteros, implementa una funci�n que ordene dicha lista de menor a mayor utilizando un algoritmo de selecci�n. Dicho algoritmo consiste en encontrar el menor de todos los elementos de la lista e intercambiarlo con el que est� en la primera posici�n. Esta operaci�n se repite recursivamente hasta finalizar la lista.
--Pr�ctica 1
--Programaci�n Declarativa
--seleccion [8,-4,0,8,-9,2,1,94]= [-9,-4,0,1,2,8,8,94]

--10. Dada una lista de enteros, implementa una funci�n que ordene dicha lista de menor a mayor utilizando un algoritmo de la burbuja. Dicho algoritmo se basa en revisar cada elemento de la lista que va a ser ordenada con el siguiente, intercambi�ndolos de posici�n si est�n en el orden equivocado. Es necesario revisar varias veces toda la lista hasta que no se necesiten m�s intercambios, lo cual significa que la lista est� ordenada.
--burbuja [8,-4,0,8,-9,2,1,94]= [-9,-4,0,1,2,8,8,94]

--11. Dada una lista de enteros, implementa una funci�n que ordene dicha lista de menor a mayor utilizando un algoritmo de Mergesort. Dicho algoritmo, basado en la t�cnica del divide y vencer�s, consiste en dividir la lista en dos partes que son ordenadas por separado y que luego son fusionadas de tal forma que la lista final quede ordenada.