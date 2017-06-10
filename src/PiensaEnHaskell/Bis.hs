
module PiensaEnHaskell.Bis where

----Ejercicio 1.22.1. Las dimensiones de los rectángulos puede representarse por pares; por ejemplo,
----(5,3) representa a un rectángulo de base 5 y altura 3. Definir la función mayorRectangulo tal
----que (mayorRectangulo r1 r2) es el rectángulo de mayor área entre r1 y r2. Por ejemplo,
----mayorRectangulo (4,6) (3,7) == (4,6)
----mayorRectangulo (4,6) (3,8) == (4,6)
----mayorRectangulo (4,6) (3,9) == (3,9)

mayorRectangulo::(Int,Int)->(Int,Int)->(Int,Int)
mayorRectangulo (x,y)(z,t)= if (compara) then (x,y) else (z,t)
	where
		compara = x*y>z*t

--DEFINICIONE POR COMPRESION

-- 2.1. Suma de los cuadrados de los n primeros números
-- es la suma de los cuadrados de los primeros n números; es decir, 12 + 22 +    + n2. 
-- sumaDeCuadrados 3 == 14
-- sumaDeCuadrados 100 == 338350

sumaDeCuadrados:: Int->Int
sumaDeCuadrados x= sum [n^2|n<-[1..x]]


-- Ejercicio 2.2.1. Definir por comprensión la función
-- tal que (replica n x) es la lista formada por n copias del elemento x. Por ejemplo,
-- replica 3 True == [True, True, True]
-- Nota: La función replica es equivalente a la predefinida replicate.

replica :: Int -> a -> [a]
replica x y= [y|_<-[1..x]]



----Ejercicio 2.3.1. Definir la función suma tal (suma n) es la suma de los n primeros números.
----Por ejemplo,
----suma 3 == 6

suma:: Int->Int
suma x= sum [x|x<-[1..x]]




----Definir la función linea tal que (linea n) es la línea n–ésima de los triángulos aritméticos.
----Por ejemplo,
----linea 4 == [7,8,9,10]
----linea 5 == [11,12,13,14,15]

linea::Int->[Int]
linea x= [n|n<-[suma(x-1)+1..suma(x)]]


----Ejercicio 2.3.3. Definir la función triangulo tal que (triangulo n) es el triángulo aritmético
----de altura n. Por ejemplo,
----triangulo 3 == [[1],[2,3],[4,5,6]]
----triangulo 4 == [[1],[2,3],[4,5,6],[7,8,9,10]]

-- MAL
triangulo::Int->[[Int]]
triangulo = triangiloAux[[]]

triangiloAux::[[Int]]->Int->[[Int]]
triangiloAux r 0 =r
triangiloAux[[x]] y = triangiloAux [[8]] (y-1) 



-- triangulo n = [linea m | m <- [1..n]]

----2.4. Números perfectos
----Ejercicio 2.4.1. Un entero positivo es perfecto si es igual a la suma de sus factores, excluyendo
----el propio número. Definir por comprensión la función
----perfectos :: Int -> [Int]
----42 Capítulo 2. Definiciones por comprensión
----tal que (perfectos n) es la lista de todos los números perfectos menores que n. Por ejemplo,
----perfectos 500 == [6,28,496]




----perfectos n = [x | x <- [1..n], sum (init (factores x)) == x]
----donde (factores n) es la lista de los factores de n
----factores :: Int -> [Int]
----factores n = [x | x <- [1..n], n 'mod' x == 0]
----2.5. Números abundantes
----Un número natural n se denomina abundante si es menor que la suma de sus divisores
----propios. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.
----Ejercicio 2.5.1. Definir la función numeroAbundante tal que (numeroAbundante n) se verifica
----si n es un número abundante. Por ejemplo,
----numeroAbundante 5 == False
----numeroAbundante 12 == True
----numeroAbundante 28 == False
----numeroAbundante 30 == True

----Ejercicio 2.5.2. Definir la función numerosAbundantesMenores tal que (numerosAbundantesMenores n)
----es la lista de números abundantes menores o iguales que n. Por ejemplo,
----numerosAbundantesMenores 50 == [12,18,20,24,30,36,40,42,48]
----Solución:
----numerosAbundantesMenores :: Int -> [Int]
----numerosAbundantesMenores n = [x | x <- [1..n], numeroAbundante x]
----2.6. Problema 1 del proyecto Euler 43
----Ejercicio 2.5.3. Definir la función todosPares tal que (todosPares n) se verifica si todos los
----números abundantes menores o iguales que n son pares. Por ejemplo,
----todosPares 10 == True
----todosPares 100 == True
----todosPares 1000 == False

----Ejercicio 2.5.4. Definir la constante primerAbundanteImpar que calcule el primer número
----natural abundante impar. Determinar el valor de dicho número.
