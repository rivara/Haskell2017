
module Ejercicios.Practicas where

--Práctica 1
--El alumno debe conocer y dominar los aspectos básicos del lenguaje de programación Haskell, así como tener un conocimiento elemental del entorno de programación EclipseGavab.
--Ejercicios prácticos:

--1. Se pide implementar en Haskell una función que calcule la suma de los n primeros términos de la siguiente serie: 1/1 – ½ + 1/3 – ¼ … -1/2i + 1/(2i+1). Se recomienda utilizar dos funciones: pares(i) e impares(i); una que calcule la suma de los términos pares y otra que calcule la suma de los términos impares hasta un término concreto.

--2. Diseña una función recursiva que reciba un número entero positivo e intercambie las posiciones de los dígitos del número situados en esas posiciones (se puede suponer que las unidades ocupan la posición 0).
--intercambiar (72549,0,3) = 79542 intercambiar (72549,1,2) = 72459

--3. Diseña una función capaz de determinar si un número entero positivo es capicúa.

--4. Utilizando la función del ejercicio anterior, diseña una función que reciba un número entero positivo y busque su familiar capicúa. Dicho familiar se encuentra sumando al número su inverso. Si el número resultante es capicúa el proceso parará, si no, se repetirá el proceso a partir del número obtenido de la suma anterior.
--famCapicua (59) => famCapicua (154) => famCapicua (605) => famCapicua (1111) => 1111
--59 + 95
--154 + 451
--605 + 506
--Práctica 1




--5. Escribe un programa que reciba un número entero mayor que 2 y que devuelva, en caso de que exista, la suma de dos números primos cuyo resultado sea el propio número. En caso de que no exista dicha pareja de números primos, puede devolver el propio número con el 0.
--sumaPrimos (12) = (1,11) sumaPrimos(17)=(17,0)

--6. Se pide implementar en Haskell una función que dada una lista de enteros, devuelva las sublistas entre cada par de ceros. Se recomienda hacer uso del operador de concatenación (++), y de las funciones head y tail. Para que la lista resultante se devuelva en el mismo orden en el que aparecían las sublistas dentro de la lista original se puede utilizar la función reverse. Por ejemplo:
--parteLista([1,4,6,3,0,5,6,0,9]) = [[1,4,6,3], [5,6], [9]]
--parteLista([0,3,4,5]) = [[], [3,4,5]]
--parteLista([2,3]) = [[2,3]]
--parteLista([]) = []

--7. Diseña una función en Haskell que reciba una lista y devuelva las listas productos de la secuencia de inserciones de los elementos en cada lista anterior (empezando por la lista vacía).
--segmentos [1,8,4,7] = [[],[1],[1,8],[1,8,4],[1,8,4,7]]

--8. Dada una lista de enteros, implementa una función que ordene dicha lista de menor a mayor utilizando un algoritmo de inserción. Dicho algoritmo consiste en recorrer la lista L, insertando cada elemento L[i] en el lugar correcto entre los elementos ya ordenados L[1] ,...,L[i-1].
--insercion [8,-4,0,8,-9,2,1,94]= [-9,-4,0,1,2,8,8,94]

--9. Dada una lista de enteros, implementa una función que ordene dicha lista de menor a mayor utilizando un algoritmo de selección. Dicho algoritmo consiste en encontrar el menor de todos los elementos de la lista e intercambiarlo con el que está en la primera posición. Esta operación se repite recursivamente hasta finalizar la lista.
--Práctica 1
--Programación Declarativa
--seleccion [8,-4,0,8,-9,2,1,94]= [-9,-4,0,1,2,8,8,94]

--10. Dada una lista de enteros, implementa una función que ordene dicha lista de menor a mayor utilizando un algoritmo de la burbuja. Dicho algoritmo se basa en revisar cada elemento de la lista que va a ser ordenada con el siguiente, intercambiándolos de posición si están en el orden equivocado. Es necesario revisar varias veces toda la lista hasta que no se necesiten más intercambios, lo cual significa que la lista está ordenada.
--burbuja [8,-4,0,8,-9,2,1,94]= [-9,-4,0,1,2,8,8,94]

--11. Dada una lista de enteros, implementa una función que ordene dicha lista de menor a mayor utilizando un algoritmo de Mergesort. Dicho algoritmo, basado en la técnica del divide y vencerás, consiste en dividir la lista en dos partes que son ordenadas por separado y que luego son fusionadas de tal forma que la lista final quede ordenada.