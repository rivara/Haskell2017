module PiensaEnHaskell.PiensaEnHaskell where



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
--IMPORTANTE
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

-- Version 1
triangulo::Int->[[Int]]
triangulo = trianguloAux[]

trianguloAux::[[Int]]->Int->[[Int]]
trianguloAux r 0 = r
trianguloAux r y = [[n|n<-[suma(y-1)+1..suma(y)]]] ++ trianguloAux r  (y-1)

-- Version 2
triangulo':: Int->[[Int]]
triangulo' n =[linea m | m <- [1..n]]

----2.4. Números perfectos
----Ejercicio 2.4.1. Un entero positivo es perfecto si es igual a la suma de sus factores, excluyendo
----el propio número. Definir por comprensión la función
----perfectos :: Int -> [Int]

----42 Capítulo 2. Definiciones por comprensión
----tal que (perfectos n) es la lista de todos los números perfectos menores que n. Por ejemplo,
----perfectos 500 == [6,28,496]

-- version 1
perfectos::Int->Bool
perfectos x= (x == sum (factores x)) 

factores::Int->[Int]
factores y= factorialAux[] (y-1) y

factorialAux::[Int]->Int->Int->[Int]
factorialAux r 0 _= r
factorialAux r x y= if (y `mod` x ==0) then factorialAux (r++[x]) (x-1) y else factorialAux r (x-1) y

-- version 2 

---- perfectos' 500 == [6,28,496]
perfectos' :: Int -> [Int]
perfectos' n = [x | x <- [1..n], sum (init (factores' x)) == x]

--donde (factores n) es la lista de los factores de n

factores' :: Int -> [Int]
factores' n = [x | x <- [1..n], n `mod` x == 0]



----2.5. Números abundantes
----Un número natural n se denomina abundante si es menor que la suma de sus divisores
----propios. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.
----Ejercicio 2.5.1. Definir la función numeroAbundante tal que (numeroAbundante n) se verifica
----si n es un número abundante. Por ejemplo,
----numeroAbundante 5 == False
----numeroAbundante 12 == True
----numeroAbundante 28 == False
----numeroAbundante 30 == True

--version 1 
numeroAbundante::Int->Bool
numeroAbundante x = x > (sum[n|n<-[1..x-1],x `mod` n == 0])

----Ejercicio 2.5.2. Definir la función numerosAbundantesMenores tal que (numerosAbundantesMenores n)
----es la lista de números abundantes menores o iguales que n. Por ejemplo,
----numerosAbundantesMenores 50 == [12,18,20,24,30,36,40,42,48]
numerosAbundantesMenores::Int->[Int]
numerosAbundantesMenores x=[n|n<-[1..x-1],x `mod` n == 0]

----2.6. Problema 1 del proyecto Euler 43
----Ejercicio 2.5.3. Definir la función todosPares tal que (todosPares n) se verifica si todos los
----números abundantes menores o iguales que n son pares. Por ejemplo,
----todosPares 10 == True
----todosPares 100 == True
----todosPares 1000 == False

todosPares::Int->Bool
todosPares x=and[even n|n <- numerosAbundantesMenores x]

----Ejercicio 2.5.4. Definir la constante primerAbundanteImpar que calcule el primer número
----natural abundante impar. Determinar el valor de dicho número.
----Su cálculo es
----primerAbundanteImpar----945
primerAbundanteImpar :: Int	
primerAbundanteImpar = head [x | x <-[1..], numeroAbundante x, odd x]

----Ejercicio 2.6.1. Definir la función
----euler1 :: Integer -> Integer
----tal que (euler1 n) es la suma de todos los múltiplos de 3 ó 5 menores que n. Por ejemplo,
----euler1 10 == 23
-- version1
euler::Int->Int
euler x = sum[n|n<-[1..x-1], x`mod`5==0||x`mod`3==0 ]

--version2
euler1::Int->Int
euler1 x=sum [n|n<-[1..x-1],multiplo x 3|| multiplo x 5]
	where multiplo x y = mod x y == 0



----Definir la función aproxE' tal que (aproxE' n) es la aproximación de e que se obtiene sumando
----los términos de la serie hasta 1
----n! . Por ejemplo,
----aproxE' 10 == 2.718281801146385
----aproxE' 100 == 2.7182818284590455
-- aproxE:: Int->[Int]
-- aproxE n = [(1+1/m)**m | m <- [1..n]]



----2.11. Ternas pitagóricas
----Ejercicio 2.11.1. Una terna (x, y, z) de enteros positivos es pitagórica si x2 + y2 = z2. Usando
----una lista por comprensión, definir la función
----
----tal que (pitagoricas n) es la lista de todas las ternas pitagóricas cuyas componentes están
----entre 1 y n. Por ejemplo,
----pitagoricas 10 == [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
--pitagoricas :: Int -> [(Int,Int,Int)]



pitagoricas :: Int -> [(Int,Int,Int)]
pitagoricas n = [(x,y,z) | x <- [1..n],y <- [1..n],z <- [1..n], x^2 + y^2 == z^2]





----Ejercicio 2.11.2. Definir la función
----numeroDePares :: (Int,Int,Int) -> Int
----tal que (numeroDePares t) es el número de elementos pares de la terna t. Por ejemplo,
----48 Capítulo 2. Definiciones por comprensión
----numeroDePares (3,5,7) == 0
----numeroDePares (3,6,7) == 1
----numeroDePares (3,6,4) == 2
----numeroDePares (4,6,4) == 3

numeroDePares::(Int,Int,Int)->Int
numeroDePares(x,y,z)= sum[1|n<-[x,y,z],even n]


----Ejercicio 2.11.3. Definir la función
----conjetura :: Int -> Bool
----tal que (conjetura n) se verifica si todas las ternas pitagóricas cuyas componentes están entre
----1 y n tiene un número impar de números pares. Por ejemplo,
----conjetura 10 == True

conjetura :: Int -> Bool
conjetura n = and [odd (numeroDePares t) | t <- pitagoricas n]







----2.12. Problema 9 del Proyecto Euler
----Ejercicio 2.12.1. Una terna pitagórica es una terna de números naturales (a, b, c) tal que a <
----b < c y a2 + b2 = c2. Por ejemplo (3, 4, 5) es una terna pitagórica. Definir la función
----ternasPitagoricas :: Integer -> [[Integer]]
----tal que (ternasPitagoricas x) es la lista de las ternas pitagóricas cuya suma es x. Por
----ternasPitagoricas 12 == [(3,4,5)]
----ternasPitagoricas 60 == [(10,24,26),(15,20,25)]

ternasPitagoricas :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas x = [(a,b,c) | a <- [1..x],
								 b <- [a+1..x],
								 c <- [x-a-b],
								 a^2 + b^2 == c^2]


----Ejercicio 2.12.2. Definir la constante euler9 tal que euler9 es producto abc donde (a, b, c) es
----la única terna pitagórica tal que a + b + c = 1000. Calcular el valor de euler9.
----Solución:
----euler9 = a*b*c
----where (a,b,c) = head (ternasPitagoricas 1000)
----El cálculo del valor de euler9 es


----2.13. Producto escalar
----Ejercicio 2.13.1. El producto escalar de dos listas de enteros xs e ys de longitud n viene dado por
----la suma de los productos de los elementos correspondientes. Definir por comprensión la función
----productoEscalar :: [Int] -> [Int] -> Int
----tal que (productoEscalar xs ys) es el producto escalar de las listas xs e ys. Por ejemplo,
----productoEscalar [1,2,3] [4,5,6] == 32

productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys = sum [x*y | (x,y) <- zip xs ys]



----2.14. Suma de pares de elementos consecutivos
----Ejercicio 2.14.1. Definir, por comprensión, la función
----sumaConsecutivos :: [Int] -> [Int]
----tal que (sumaConsecutivos xs) es la suma de los pares de elementos consecutivos de la lista
----xs. Por ejemplo,
----sumaConsecutivos [3,1,5,2] == [4,6,7]
----sumaConsecutivos [3] == []

sumaConsecutivos::[Int]->[Int]
sumaConsecutivos xs =[x+y|(x,y)<- zip xs(tail xs)]





----2.15. Posiciones de un elemento en una lista
----Ejercicio 2.15.1. En el tema se ha definido la función
----posiciones :: Eq a => a -> [a] -> [Int]
----tal que (posiciones x xs) es la lista de las posiciones ocupadas por el elemento x en la lista
----xs. Por ejemplo,
----posiciones 5 [1,5,3,5,5,7] == [1,3,4]
----Definir, usando la función busca (definida en el tema 5), la función
----posiciones' :: Eq a => a -> [a] -> [Int]
----tal que posiciones' sea equivalente a posiciones.

-- version1 recusrividad final


-- ? Error raro hashkell
--posiciones::Eq a =>a->[a]->[Int]
--posiciones x xs= posicionesAux x xs [] 0

--posicionesAux::Eq a =>a->[a]->[Int]->Int->[Int]
--posicionesAux _ [] r _= r
--posicionesAux x (y:ys) r c= if (x==y)then  posicionesAuxelse x ys (r++[c]) (c+1)   posicionesAux x ys r (c+1) 


-- version2
posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs =[i | (x',i) <- zip xs [0..n], x == x']
	where n = length xs - 1

----Solución: La definición de posiciones es
----posiciones :: Eq a => a -> [a] -> [Int]
----posiciones x xs =
----[i | (x',i) <- zip xs [0..n], x == x']
----where n = length xs - 1
----La definición de busca es
----busca :: Eq a => a -> [(a, b)] -> [b]
----busca c t = [v | (c', v) <- t, c' == c]


----La redefinición de posiciones es
----posiciones' :: Eq a => a -> [a] -> [Int]
----posiciones' x xs = busca x (zip xs [0..])




----2.16. Representación densa de un polinomio representado dispersamente 51
----2.16. Representación densa de un polinomio representado
----dispersamente
----Ejercicio 2.16.1. Los polinomios pueden representarse de forma dispersa o densa. Por ejemplo,
----el polinomio 6x4 ?? 5x2 + 4x ?? 7 se puede representar de forma dispersa por [6,0,-5,4,-7] y
----de forma densa por [(4,6),(2,-5),(1,4),(0,-7)]. Definir la función
----densa :: [Int] -> [(Int,Int)]
----tal que (densa xs) es la representación densa del polinomio cuya representación dispersa es xs.
----Por ejemplo,
----densa [6,0,-5,4,-7] == [(4,6),(2,-5),(1,4),(0,-7)]
----densa [6,0,0,3,0,4] == [(5,6),(2,3),(0,4)]

densa :: [Int] -> [(Int,Int)]
densa xs = [(x,y) | (x,y) <- zip [n-1,n-2..0] xs, y /= 0]
	where n = length xs


----2.17. Producto cartesiano
----Ejercicio 2.17.1. La función
----pares :: [a] -> [b] -> [(a,b)]
----definida por
----pares xs ys = [(x,y) | x <- xs, y <- ys]
----toma como argumento dos listas y devuelve la listas de los pares con el primer elemento de la
----primera lista y el segundo de la segunda. Por ejemplo,
----ghci> pares [1..3] [4..6]
----[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
----Definir, usando dos listas por comprensión con un generador cada una, la función
----pares' :: [a] -> [b] -> [(a,b)]
----tal que pares' sea equivalente a pares.
----Indicación: Utilizar la función predefinida concat y encajar una lista por comprensión
----dentro de la otra.

pares' :: [a] -> [b] -> [(a,b)]
pares' xs ys = concat [[(x,y) | y <- ys] | x <- xs]



----2.18. Consulta de bases de datos
----La bases de datos sobre actividades de personas pueden representarse mediante listas
----de elementos de la forma (a, b, c, d), donde a es el nombre de la persona, b su actividad,
----c su fecha de nacimiento y d la de su fallecimiento. Un ejemplo es la siguiente que
----usaremos a lo largo de los siguientes ejercicios

personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
	("Velazquez","Pintura",1599,1660),
	("Picasso","Pintura",1881,1973),
	("Beethoven","Musica",1770,1823),
	("Poincare","Ciencia",1854,1912),
	("Quevedo","Literatura",1580,1654),
	("Goya","Pintura",1746,1828),
	("Einstein","Ciencia",1879,1955),
	("Mozart","Musica",1756,1791),
	("Botticelli","Pintura",1445,1510),
	("Borromini","Arquitectura",1599,1667),
	("Bach","Musica",1685,1750)]


----Ejercicio 2.18.1. Definir la función nombres tal que (nombres bd) es la lista de los nombres
----de las personas de la base de datos bd. Por ejemplo,
----ghci> nombres personas["Cervantes","Velazquez","Picasso","Beethoven","Poincare","Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini","Bach"]

nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [x | (x,_,_,_) <- bd]


----Ejercicio 2.18.2. Definir la función musicos tal que (musicos bd) es la lista de los nombres
----de los músicos de la base de datos bd. Por ejemplo,
----ghci> musicos personas
----["Beethoven","Mozart","Bach"]

musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [x | (x,m,_,_) <- bd, m == "Musica"]
----
----2.18. Consulta de bases de datos 53
----Ejercicio 2.18.3. Definir la función seleccion tal que (seleccion bd m) es la lista de los
----nombres de las personas de la base de datos bd cuya actividad es m. Por ejemplo,
----ghci> seleccion personas "Pintura"
----["Velazquez","Picasso","Goya","Botticelli"]
----Solución:
----seleccion :: [(String,String,Int,Int)] -> String -> [String]
----seleccion bd m = [ x | (x,m',_,_) <- bd, m == m' ]
----Ejercicio 2.18.4. Definir, usando el apartado anterior, la función musicos' tal que (musicos' bd)
----es la lista de los nombres de los músicos de la base de datos bd. Por ejemplo,
----ghci> musicos' personas
----["Beethoven","Mozart","Bach"]

--musicos' :: [(String,String,Int,Int)] -> [String]
--musicos' bd = seleccion bd "Musica"
----Ejercicio 2.18.5. Definir la función vivas tal que (vivas bd a) es la lista de los nombres de
----las personas de la base de datos bd que estaban vivas en el año a. Por ejemplo,
----ghci> vivas personas 1600
----["Cervantes","Velazquez","Quevedo","Borromini"]

vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas ps a = [x | (x,_,a1,a2) <- ps, a1 <= a, a <= a2]
----Nota. Un caso de estudio para las definiciones por comprensión es el capítulo 15 “El
----cifrado César” (página 325)




----------- definidiones por comprension
----
----3.1. Potencia de exponente natural
----Ejercicio 3.1.1. Definir por recursión la función
----potencia :: Integer -> Integer -> Integer
----tal que (potencia x n) es x elevado al número natural n. Por ejemplo,
----potencia 2 3 == 8

potencia::Int->Int->Int
potencia= potenciaAux 1

potenciaAux::Int->Int->Int->Int
potenciaAux r _ 0 =r 
potenciaAux r c x= potenciaAux  (r*c) c (x-1)

-------- version 2

potencia :: Integer -> Integer -> Integer
potencia m 0 = 1
potencia m n = m*(potencia m (n-1))



----3.2. Replicación de un elemento
----Ejercicio 3.2.1. Definir por recursión la función
----replicate' :: Int -> a -> [a]
----tal que (replicate' n x) es la lista formado por n copias del elemento x. Por ejemplo,
----replicate' 3 2 == [2,2,2]


replicate' :: Int -> a -> [a]
replicate' =  replicateAux []

replicateAux::[a]->Int -> a -> [a]
replicateAux r 0 _= r
replicateAux r n x = replicateAux (x:r) (n-1) x

------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------

-- MARTES MAÑANA

----3.3. Doble factorial 57
----3.3. Doble factorial
----Ejercicio 3.3.1. El doble factorial de un número n se define por
----0!! = 1
----1!! = 1
----n!! = n*(n-2)* ... * 3 * 1, si n es impar
----n!! = n*(n-2)* ... * 4 * 2, si n es par
----Por ejemplo,
----8!! = 8*6*4*2 = 384
----9!! = 9*7*5*3*1 = 945
----Definir, por recursión, la función
----dobleFactorial :: Integer -> Integer
----tal que (dobleFactorial n) es el doble factorial de n. Por ejemplo,
----dobleFactorial 8 == 384
----dobleFactorial 9 == 945



----3.5. Menor número divisible por una sucesión de números
----Los siguientes ejercicios tienen como objetivo resolver el problema 5 del proyecto
----Euler que consiste en calcular el menor número divisible por los números del 1 al 20.
----Ejercicio 3.5.1. Definir por recursión la función
----menorDivisible :: Integer -> Integer -> Integer
----tal que (menorDivisible a b) es el menor número divisible por los números del a al b. Por
----ejemplo,
----menorDivisible 2 5 == 60
----Indicación: Usar la función lcm tal que (lcm x y) es el mínimo común múltiplo de x e y.
----Solución:
----menorDivisible :: Integer -> Integer -> Integer
----menorDivisible a b
--- | a == b = a
--- | otherwise = lcm a (menorDivisible (a+1) b)
----Ejercicio 3.5.2. Definir la constante
----euler5 :: Integer
----tal que euler5 es el menor número divisible por los números del 1 al 20 y calcular su valor.
----Solución:
----euler5 :: Integer
----euler5 = menorDivisible 1 20
----El cálculo es
----ghci> euler5
----232792560




----3.9. Último elemento de una lista
----Ejercicio 3.9.1. Definir por recursión la función
----last' :: [a] -> a
----tal que (last xs) es el último elemento de xs. Por ejemplo,
----last' [2,3,5] => 5
----Solución:
----last' :: [a] -> a
----last' [x] = x
----last' (_:xs) = last' xs
----3.10. Concatenación de una lista 61
----3.10. Concatenación de una lista
----Ejercicio 3.10.1. Definir por recursión la función
----concat' :: [[a]] -> [a]
----tal que (concat' xss) es la lista obtenida concatenando las listas de xss. Por ejemplo,
----concat' [[1..3],[5..7],[8..10]] == [1,2,3,5,6,7,8,9,10]

----3.11. Selección de un elemento
----Ejercicio 3.11.1. Definir por recursión la función
----selecciona :: [a] -> Int -> a
----tal que (selecciona xs n) es el n–ésimo elemento de xs. Por ejemplo,
----selecciona [2,3,5,7] 2 == 5


----3.12. Selección de los primeros elementos
----Ejercicio 3.12.1. Definir por recursión la función



----take' :: Int -> [a] -> [a]
----tal que (take' n xs) es la lista de los n primeros elementos de xs. Por ejemplo,
----take' 3 [4..12] == [4,5,6]
----Solución:
----take' :: Int -> [a] -> [a]
----take' 0 _ = []
----take' _ [] = []
----take' n (x:xs) = x : take' (n-1) xs
----62 Capítulo 3. Definiciones por recursión
----3.13. Intercalación de la media aritmética
----Ejercicio 3.13.1. Definir la función
----refinada :: [Float] -> [Float]
----tal que (refinada xs) es la lista obtenida intercalando entre cada dos elementos consecutivos
----de xs su media aritmética. Por ejemplo,
----refinada [2,7,1,8] == [2.0,4.5,7.0,4.0,1.0,4.5,8.0]
----refinada [2] == [2.0]
----refinada [] == []

----3.14.5. La ordenación por mezcla da una permutación
----Ejercicio 3.14.6. Definir por recursión la función
----borra :: Eq a => a -> [a] -> [a]
----tal que (borra x xs) es la lista obtenida borrando una ocurrencia de x en la lista xs. Por
----ejemplo,
----borra 1 [1,2,1] == [2,1]
----borra 3 [1,2,1] == [1,2,1]
----Solución:
----borra :: Eq a => a -> [a] -> [a]
----borra x [] = []
----borra x (y:ys) | x == y = ys
--- |otherwise = y : borra x ys
----3.14. Ordenación por mezcla 65
----3.14.6. Determinación de permutaciones
----Ejercicio 3.14.7. Definir por recursión la función
----esPermutacion :: Eq a => [a] -> [a] -> Bool
----tal que (esPermutacion xs ys) se verifica si xs es una permutación de ys. Por ejemplo,
----esPermutacion [1,2,1] [2,1,1] == True
----esPermutacion [1,2,1] [1,2,2] == False
----Solución:
----esPermutacion :: Eq a => [a] -> [a] -> Bool
----esPermutacion [] [] = True
----esPermutacion [] (y:ys) = False
----esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (borra x ys)
----Ejercicio 3.14.8. Comprobar con QuickCheck que la ordenación por mezcla de una lista es una
----permutación de la lista.
----Solución: La propiedad es
----prop_ordMezcla_pemutacion :: Ord a => [a] -> Bool
----prop_ordMezcla_pemutacion xs = esPermutacion (ordMezcla xs) xs
----La comprobación es
----ghci> quickCheck prop_ordMezcla_permutacion
---- +++ OK, passed 100 tests.
----


----------- comprension y recursion
----
----4.1. Suma de los cuadrados de los primeros números
----Ejercicio 4.1.1. Definir, por recursión; la función
----4.1. Suma de los cuadrados de los primeros números 69
----sumaCuadradosR :: Integer -> Integer
----tal que (sumaCuadradosR n) es la suma de los cuadrados de los números de 1 a n. Por ejemplo,
----sumaCuadradosR 4 == 30
----Solución:
----sumaCuadradosR :: Integer -> Integer
----sumaCuadradosR 0 = 0
----sumaCuadradosR n = n^2 + sumaCuadradosR (n-1)
----Ejercicio 4.1.2. Comprobar con QuickCheck si (sumaCuadradosR n) es igual a n(n+1)(2n+1)
----6 .
----Solución: La propiedad es
----prop_SumaCuadrados n =
----n >= 0 ==>
----sumaCuadradosR n == n * (n+1) * (2*n+1) 'div' 6
----La comprobación es
----ghci> quickCheck prop_SumaCuadrados
----OK, passed 100 tests.
----Ejercicio 4.1.3. Definir, por comprensión, la función
----sumaCuadradosC :: Integer -> Integer
----tal que (sumaCuadradosC n) es la suma de los cuadrados de los números de 1 a n. Por ejemplo,
----sumaCuadradosC 4 == 30
----Solución:
----sumaCuadradosC :: Integer -> Integer
----sumaCuadradosC n = sum [x^2 | x <- [1..n]]
----Ejercicio 4.1.4. Comprobar con QuickCheck que las funciones sumaCuadradosR y sumaCuadradosC
----son equivalentes sobre los números naturales.
----Solución: La propiedad es
----prop_sumaCuadrados n =
----n >= 0 ==> sumaCuadradosR n == sumaCuadradosC n
----La comprobación es
----ghci> quickCheck prop_sumaCuadrados
---- +++ OK, passed 100 tests.
----70 Capítulo 4. Definiciones por recursión y por comprensión
----4.2. Número de bloques de escaleras triangulares
----Ejercicio 4.2.1. Se quiere formar una escalera con bloques cuadrados, de forma que tenga un número
----determinado de escalones. Por ejemplo, una escalera con tres escalones tendría la siguiente
----forma:
----XX
----XXXX
----XXXXXX
----Definir, por recursión, la función
----numeroBloquesR :: Integer -> Integer
----tal que (numeroBloquesR n) es el número de bloques necesarios para construir una escalera
----con n escalones. Por ejemplo,
----numeroBloquesR 1 == 2
----numeroBloquesR 3 == 12
----numeroBloquesR 10 == 110
----Solución:
----numeroBloquesR :: Integer -> Integer
----numeroBloquesR 0 = 0
----numeroBloquesR n = 2*n + numeroBloquesR (n-1)
----Ejercicio 4.2.2. Definir, por comprensión, la función
----numeroBloquesC :: Integer -> Integer
----tal que (numeroBloquesC n) es el número de bloques necesarios para construir una escalera
----con n escalones. Por ejemplo,
----numeroBloquesC 1 == 2
----numeroBloquesC 3 == 12
----numeroBloquesC 10 == 110
----Solución:
----numeroBloquesC :: Integer -> Integer
----numeroBloquesC n = sum [2*x | x <- [1..n]]
----Ejercicio 4.2.3. Comprobar con QuickCheck que (numeroBloquesC n) es igual a n + n2.
----Solución: La propiedad es
----4.3. Suma de los cuadrados de los impares entre los primeros números 71
----prop_numeroBloques n =
----n > 0 ==> numeroBloquesC n == n+n^2
----La comprobación es
----ghci> quickCheck prop_numeroBloques
---- +++ OK, passed 100 tests.
----4.3. Suma de los cuadrados de los impares entre los primeros
----números
----Ejercicio 4.3.1. Definir, por recursión, la función
----sumaCuadradosImparesR :: Integer -> Integer
----tal que (sumaCuadradosImparesR n) es la suma de los cuadrados de los números impares desde
----1 hasta n. Por ejemplo,
----sumaCuadradosImparesR 1 == 1
----sumaCuadradosImparesR 7 == 84
----sumaCuadradosImparesR 4 == 10
----Solución:
----sumaCuadradosImparesR :: Integer -> Integer
----sumaCuadradosImparesR 1 = 1
----sumaCuadradosImparesR n
--- |odd n = n^2 + sumaCuadradosImparesR (n-1)
--- |otherwise = sumaCuadradosImparesR (n-1)
----Ejercicio 4.3.2. Definir, por comprensión, la función
----sumaCuadradosImparesC :: Integer -> Integer
----tal que (sumaCuadradosImparesC n) es la suma de los cuadrados de los números impares desde
----1 hasta n. Por ejemplo,
----sumaCuadradosImparesC 1 == 1
----sumaCuadradosImparesC 7 == 84
----sumaCuadradosImparesC 4 == 10
----Solución:
----72 Capítulo 4. Definiciones por recursión y por comprensión
----sumaCuadradosImparesC :: Integer -> Integer
----sumaCuadradosImparesC n = sum [x^2 | x <- [1..n], odd x]
----Otra definición más simple es
----sumaCuadradosImparesC' :: Integer -> Integer
----sumaCuadradosImparesC' n = sum [x^2 | x <- [1,3..n]]
----4.4. Operaciones con los dígitos de los números
----4.4.1. Lista de los dígitos de un número
----Ejercicio 4.4.1. Definir, por recursión, la función
----digitosR :: Integer -> [Int]
----tal que (digitosR n) es la lista de los dígitos del número n. Por ejemplo,
----digitosR 320274 == [3,2,0,2,7,4]
----Solución:
----digitosR :: Integer -> [Integer]
----digitosR n = reverse (digitosR' n)
----digitosR' n
--- |n < 10 = [n]
--- |otherwise = (n 'rem' 10) : digitosR' (n 'div' 10)
----Ejercicio 4.4.2. Definir, por comprensión, la función
----digitosC :: Integer -> [Int]
----tal que (digitosC n) es la lista de los dígitos del número n. Por ejemplo,
----digitosC 320274 == [3,2,0,2,7,4]
----Indicación: Usar las funciones show y read.
----Solución:
----digitosC :: Integer -> [Integer]
----digitosC n = [read [x] | x <- show n]
----4.4. Operaciones con los dígitos de los números 73
----Ejercicio 4.4.3. Comprobar con QuickCheck que las funciones digitosR y digitos son equivalentes.
----Solución: La propiedad es
----prop_dígitos n =
----n >= 0 ==>
----digitosR n == digitosC n
----La comprobación es
----ghci> quickCheck prop_dígitos
---- +++ OK, passed 100 tests.
----4.4.2. Suma de los dígitos de un número
----Ejercicio 4.4.4. Definir, por recursión, la función
----sumaDigitosR :: Integer -> Integer
----tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
----sumaDigitosR 3 == 3
----sumaDigitosR 2454 == 15
----sumaDigitosR 20045 == 11
----Solución:
----sumaDigitosR :: Integer -> Integer
----sumaDigitosR n
--- |n < 10 = n
--- |otherwise = n 'rem' 10 + sumaDigitosR (n 'div' 10)
----Ejercicio 4.4.5. Definir, sin usar recursión, la función
----sumaDigitosNR :: Integer -> Integer
----tal que (sumaDigitosNR n) es la suma de los dígitos de n. Por ejemplo,
----sumaDigitosNR 3 == 3
----sumaDigitosNR 2454 == 15
----sumaDigitosNR 20045 == 11
----Solución:
----74 Capítulo 4. Definiciones por recursión y por comprensión
----sumaDigitosNR :: Integer -> Integer
----sumaDigitosNR n = sum (digitosR n)
----Ejercicio 4.4.6. Comprobar con QuickCheck que las funciones sumaDigitosR y sumaDigitosNR
----son equivalentes.
----Solución: La propiedad es
----prop_sumaDígitos n =
----n >= 0 ==>
----sumaDigitosR n == sumaDigitosNR n
----La comprobación es
----ghci> quickCheck prop_sumaDígitos
---- +++ OK, passed 100 tests.
----4.4.3. Decidir si es un dígito del número
----Ejercicio 4.4.7. Definir la función
----esDigito :: Integer -> Integer -> Bool
----tal que (esDigito x n) se verifica si x es una dígito de n. Por ejemplo,
----esDigito 4 1041 == True
----esDigito 3 1041 == False
----Solución:
----esDigito :: Integer -> Integer -> Bool
----esDigito x n = elem x (digitosR n)
----4.4.4. Número de dígitos de un número
----Ejercicio 4.4.8. Definir la función
----numeroDeDigitos :: Integer -> Integer
----tal que (numeroDeDigitos x) es el número de dígitos de x. Por ejemplo,
----numeroDeDigitos 34047 == 5
----Solución:
----numeroDeDigitos :: Integer -> Int
----numeroDeDigitos x = length (digitosR x)
----4.4. Operaciones con los dígitos de los números 75
----4.4.5. Número correspondiente a una lista de dígitos
----Ejercicio 4.4.9. Definir, por recursión, la función
----listaNumeroR :: [Integer] -> Integer
----tal que (listaNumeroR xs) es el número formado por los dígitos de la lista xs. Por ejemplo,
----listaNumeroR [5] == 5
----listaNumeroR [1,3,4,7] == 1347
----listaNumeroR [0,0,1] == 1
----Solución:
----listaNumeroR :: [Integer] -> Integer
----listaNumeroR xs = listaNumeroR' (reverse xs)
----listaNumeroR' :: [Integer] -> Integer
----listaNumeroR' [x] = x
----listaNumeroR' (x:xs) = x + 10 * (listaNumeroR' xs)
----Ejercicio 4.4.10. Definir, por comprensión, la función
----listaNumeroC :: [Integer] -> Integer
----tal que (listaNumeroC xs) es el número formado por los dígitos de la lista xs. Por ejemplo,
----listaNumeroC [5] == 5
----listaNumeroC [1,3,4,7] == 1347
----listaNumeroC [0,0,1] == 1
----Solución:
----listaNumeroC :: [Integer] -> Integer
----listaNumeroC xs = sum [y*10^n | (y,n) <- zip (reverse xs) [0..]]
----4.4.6. Concatenación de dos números
----Ejercicio 4.4.11. Definir, por recursión, la función
----pegaNumerosR :: Integer -> Integer -> Integer
----tal que (pegaNumerosR x y) es el número resultante de “pegar” los números x e y. Por ejemplo,
----pegaNumerosR 12 987 == 12987
----pegaNumerosR 1204 7 == 12047
----pegaNumerosR 100 100 == 100100
----76 Capítulo 4. Definiciones por recursión y por comprensión
----Solución:
----pegaNumerosR :: Integer -> Integer -> Integer
----pegaNumerosR x y
--- |y < 10 = 10*x+y
--- |otherwise = 10 * pegaNumerosR x (y 'div'10) + (y 'rem' 10)
----Ejercicio 4.4.12. Definir, sin usar recursión, la función
----pegaNumerosNR :: Integer -> Integer -> Integer
----tal que (pegaNumerosNR x y) es el número resultante de “pegar” los números x e y. Por ejemplo,
----pegaNumerosNR 12 987 == 12987
----pegaNumerosNR 1204 7 == 12047
----pegaNumerosNR 100 100 == 100100
----Solución:
----pegaNumerosNR :: Integer -> Integer -> Integer
----pegaNumerosNR x y = listaNumeroC (digitosR x ++ digitosR y)
----Ejercicio 4.4.13. Comprobar con QuickCheck que las funciones pegaNumerosR y pegaNumerosNR
----son equivalentes.
----Solución: La propiedad es
----prop_pegaNumeros x y =
----x >= 0 && y >= 0 ==>
----pegaNumerosR x y == pegaNumerosNR x y
----La comprobción es
----ghci> quickCheck prop_pegaNumeros
---- +++ OK, passed 100 tests.
----4.4.7. Primer dígito de un número
----Ejercicio 4.4.14. Definir, por recursión, la función
----primerDigitoR :: Integer -> Integer
----tal que (primerDigitoR n) es el primer dígito de n. Por ejemplo,
----4.4. Operaciones con los dígitos de los números 77
----primerDigitoR 425 == 4
----Solución:
----primerDigitoR :: Integer -> Integer
----primerDigitoR n
--- |n < 10 = n
--- |otherwise = primerDigitoR (n 'div' 10)
----Ejercicio 4.4.15. Definir, sin usar recursión, la función
----primerDigitoNR :: Integer -> Integer
----tal que (primerDigitoNR n) es el primer dígito de n. Por ejemplo,
----primerDigitoNR 425 == 4
----Solución:
----primerDigitoNR :: Integer -> Integer
----primerDigitoNR n = head (digitosR n)
----Ejercicio 4.4.16. Comprobar con QuickCheck que las funciones primerDigitoR y primerDigitoNR
----son equivalentes.
----Solución: La propiedad es
----prop_primerDigito x =
----x >= 0 ==>
----primerDigitoR x == primerDigitoNR x
----La comprobación es
----ghci> quickCheck prop_primerDigito
---- +++ OK, passed 100 tests.
----4.4.8. Último dígito de un número
----Ejercicio 4.4.17. Definir la función
----ultimoDigito :: Integer -> Integer
----tal que (ultimoDigito n) es el último dígito de n. Por ejemplo,
----ultimoDigito 425 == 5
----Solución:
----ultimoDigito :: Integer -> Integer
----ultimoDigito n = n 'rem' 10
----78 Capítulo 4. Definiciones por recursión y por comprensión
----4.4.9. Número con los dígitos invertidos
----Ejercicio 4.4.18. Definir la función
----inverso :: Integer -> Integer
----tal que (inverso n) es el número obtenido escribiendo los dígitos de n en orden inverso. Por
----ejemplo,
----inverso 42578 == 87524
----inverso 203 == 302
----Solución:
----inverso :: Integer -> Integer
----inverso n = listaNumeroC (reverse (digitosR n))
----Ejercicio 4.4.19. Definir, usando show y read, la función
----inverso' :: Integer -> Integer
----tal que (inverso' n) es el número obtenido escribiendo los dígitos de n en orden inverso’. Por
----ejemplo,
----inverso' 42578 == 87524
----inverso' 203 == 302
----Solución:
----inverso' :: Integer -> Integer
----inverso' n = read (reverse (show n))
----Ejercicio 4.4.20. Comprobar con QuickCheck que las funciones inverso e inverso' son equivalentes.
----Solución: La propiedad es
----prop_inverso n =
----n >= 0 ==>
----inverso n == inverso' n
----La comprobación es
----ghci> quickCheck prop_inverso
---- +++ OK, passed 100 tests.
----4.4. Operaciones con los dígitos de los números 79
----4.4.10. Decidir si un número es capicúa
----Ejercicio 4.4.21. Definir la función
----capicua :: Integer -> Bool
----tal que (capicua n) se verifica si los dígitos de n son los mismos de izquierda a derecha que de
----derecha a izquierda. Por ejemplo,
----capicua 1234 = False
----capicua 1221 = True
----capicua 4 = True
----Solución:
----capicua :: Integer -> Bool
----capicua n = n == inverso n
----4.4.11. Suma de los dígitos de 21000
----En el problema 16 del proyecto Euler1 se pide calcular la suma de las dígitos de 21000.
----Lo resolveremos mediante los distintos apartados de este ejercicio.
----Ejercicio 4.4.22. Definir la función
----euler16 :: Integer -> Integer
----tal que (euler16 n) es la suma de los dígitos de 2n. Por ejemplo,
----euler16 4 == 7
----Solución:
----euler16 :: Integer -> Integer
----euler16 n = sumaDigitosNR (2^n)
----Ejercicio 4.4.23. Calcular la suma de los dígitos de 21000.
----Solución: El cálculo es
----ghci> euler16 1000
----1366
----1http://projecteuler.net/problem=16
----80 Capítulo 4. Definiciones por recursión y por comprensión
----4.4.12. Primitivo de un número
----Ejercicio 4.4.24. En el enunciado de uno de los problemas de las Olimpiadas matemáticas de
----Brasil se define el primitivo de un número como sigue:
----Dado un número natural n, multiplicamos todos sus dígitos, repetimos este procedimiento
----hasta que quede un solo dígito al cual llamamos primitivo de n. Por ejemplo
----para 327 : 3  2  7 = 42 y 4  2 = 8. Por lo tanto, el primitivo de 327 es 8.
----Definir la función
----primitivo :: Integer -> Integer
----tal que (primitivo n) es el primitivo de n. Por ejemplo.
----primitivo 327 == 8
----Solución:
----primitivo :: Integer -> Integer
----primitivo n | n < 10 = n
--- |otherwise = primitivo (producto n)
----donde (producto n) es el producto de los dígitos de n. Por ejemplo,
----producto 327 == 42
----producto :: Integer -> Integer
----producto = product . digitosC
----4.4.13. Números con igual media de sus dígitos
----Ejercicio 4.4.25. Dos números son equivalentes si la media de sus dígitos son iguales. Por
----ejemplo, 3205 y 41 son equivalentes ya que
----3 + 2 + 0 + 5
----4
---- =
----4 + 1
----2
----Definir la función
----equivalentes :: Int -> Int -> Bool
----tal que (equivalentes x y) se verifica si los números x e y son equivalentes. Por ejemplo,
----equivalentes 3205 41 == True
----equivalentes 3205 25 == False
----4.4. Operaciones con los dígitos de los números 81
----Solución:
----equivalentes :: Integer -> Integer -> Bool
----equivalentes x y = media (digitosC x) == media (digitosC y)
----donde (media xs) es la media de la lista xs. Por ejemplo,
----media [3,2,0,5] == 2.5
----media :: [Integer] -> Float
----media xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))
----4.4.14. Números con dígitos duplicados en su cuadrado
----Ejercicio 4.4.26. Un número x es especial si el número de ocurrencia de cada dígito d de x en
----x2 es el doble del número de ocurrencia de d en x. Por ejemplo, 72576 es especial porque tiene un
----2, un 5, un 6 y dos 7 y su cuadrado es 5267275776 que tiene exactamente dos 2, dos 5, dos 6 y
----cuatro 7.
----Definir la función
----especial :: Integer -> Bool
----tal que (especial x) se verifica si x es un número especial. Por ejemplo,
----especial 72576 == True
----especial 12 == False
----Calcular el menor número especial mayor que 72576.
----Solución:
----especial :: Integer -> Bool
----especial x =
----sort (ys ++ ys) == sort (show (x^2))
----where ys = show x
----El cálculo es
----ghci> head [x | x <- [72577..], especial x]
----406512
----82 Capítulo 4. Definiciones por recursión y por comprensión
----4.5. Cuadrados de los elementos de una lista
----Ejercicio 4.5.1. Definir, por comprensión, la función
----cuadradosC :: [Integer] -> [Integer]
----tal que (cuadradosC xs) es la lista de los cuadrados de xs. Por ejemplo,
----cuadradosC [1,2,3] == [1,4,9]
----Solución:
----cuadradosC :: [Integer] -> [Integer]
----cuadradosC xs = [x*x | x <- xs]
----Ejercicio 4.5.2. Definir, por recursión, la función
----cuadradosR :: [Integer] -> [Integer]
----tal que (cuadradosR xs) es la lista de los cuadrados de xs. Por ejemplo,
----cuadradosR [1,2,3] == [1,4,9]
----Solución:
----cuadradosR :: [Integer] -> [Integer]
----cuadradosR [] = []
----cuadradosR (x:xs) = x*x : cuadradosR xs
----Ejercicio 4.5.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Solución: La propiedad es
----prop_cuadrados :: [Integer] -> Bool
----prop_cuadrados xs =
----cuadradosC xs == cuadradosR xs
----La comprobación es
----ghci> quickCheck prop_cuadrados
---- +++ OK, passed 100 tests.
----4.6. Números impares de una lista 83
----4.6. Números impares de una lista
----Ejercicio 4.6.1. Definir, por comprensión, la función
----imparesC :: [Integer] -> [Integer]
----tal que (imparesC xs) es la lista de los números impares de xs. Por ejemplo,
----imparesC [1,2,4,3,6] == [1,3]
----Solución:
----imparesC :: [Integer] -> [Integer]
----imparesC xs = [x | x <- xs, odd x]
----Ejercicio 4.6.2. Definir, por recursión, la función
----imparesR :: [Integer] -> [Integer]
----tal que (imparesR xs) es la lista de los números impares de xs. Por ejemplo,
----imparesR [1,2,4,3,6] == [1,3]
----Solución:
----imparesR :: [Integer] -> [Integer]
----imparesR [] = []
----imparesR (x:xs) | odd x = x : imparesR xs
--- |otherwise = imparesR xs
----Ejercicio 4.6.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Solución: La propiedad es
----prop_impares :: [Integer] -> Bool
----prop_impares xs =
----imparesC xs == imparesR xs
----La comprobación es
----ghci> quickCheck prop_impares
---- +++ OK, passed 100 test
----
--
--
-------CADENAS
----
----5.1. Suma de los dígitos de una cadena
----Ejercicio 5.1.1. Definir, por comprensión, la función
----sumaDigitosC :: String -> Int
----tal que (sumaDigitosC xs) es la suma de los dígitos de la cadena xs. Por ejemplo,
----103
----104 Capítulo 5. Funciones sobre cadenas
----sumaDigitosC "SE 2431 X" == 10
----Nota: Usar las funciones isDigit y digitToInt.
----Solución:
----sumaDigitosC :: String -> Int
----sumaDigitosC xs = sum [digitToInt x | x <- xs, isDigit x]
----Ejercicio 5.1.2. Definir, por recursión, la función
----sumaDigitosR :: String -> Int
----tal que (sumaDigitosR xs) es la suma de los dígitos de la cadena xs. Por ejemplo,
----sumaDigitosR "SE 2431 X" == 10
----Nota: Usar las funciones isDigit y digitToInt.
----Solución:
----sumaDigitosR :: String -> Int
----sumaDigitosR [] = 0
----sumaDigitosR (x:xs)
--- |isDigit x = digitToInt x + sumaDigitosR xs
--- |otherwise = sumaDigitosR xs
----Ejercicio 5.1.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Solución: La propiedad es
----prop_sumaDigitos :: String -> Bool
----prop_sumaDigitos xs =
----sumaDigitosC xs == sumaDigitosR xs
----La comprobación es
----ghci> quickCheck prop_sumaDigitos
---- +++ OK, passed 100 tests.
----5.2. Capitalización de una cadena 105
----5.2. Capitalización de una cadena
----Ejercicio 5.2.1. Definir, por comprensión, la función
----mayusculaInicial :: String -> String
----tal que (mayusculaInicial xs) es la palabra xs con la letra inicial en mayúscula y las restantes
----en minúsculas. Por ejemplo,
----mayusculaInicial "sEviLLa" == "Sevilla"
----Nota: Usar las funciones toLowery toUpper.
----Solución:
----mayusculaInicial :: String -> String
----mayusculaInicial [] = []
----mayusculaInicial (x:xs) = toUpper x : [toLower x | x <- xs]
----Ejercicio 5.2.2. Definir, por recursión, la función
----mayusculaInicialR :: String -> String
----tal que (mayusculaInicialR xs) es la palabra xs con la letra inicial en mayúscula y las restantes
----en minúsculas. Por ejemplo,
----mayusculaInicialR "sEviLLa" == "Sevilla"
----Solución:
----mayusculaInicialR :: String -> String
----mayusculaInicialR [] = []
----mayusculaInicialR (x:xs) = toUpper x : aux xs
----where aux (x:xs) = toLower x : aux xs
----aux [] = []
----Ejercicio 5.2.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Solución: La propiedad es
----prop_mayusculaInicial :: String -> Bool
----prop_mayusculaInicial xs =
----mayusculaInicial xs == mayusculaInicialR xs
----La comprobación es
----ghci> quickCheck prop_mayusculaInicial
---- +++ OK, passed 100 tests.
----106 Capítulo 5. Funciones sobre cadenas
----5.3. Título con las reglas de mayúsculas iniciales
----Ejercicio 5.3.1. Se consideran las siguientes reglas de mayúsculas iniciales para los títulos:
----la primera palabra comienza en mayúscula y
----todas las palabras que tienen 4 letras como mínimo empiezan con mayúsculas.
----Definir, por comprensión, la función
----titulo :: [String] -> [String]
----tal que (titulo ps) es la lista de las palabras de ps con las reglas de mayúsculas iniciales de
----los títulos. Por ejemplo,
----ghci> titulo ["eL","arTE","DE","La","proGraMacion"]
----["El","Arte","de","la","Programacion"]
----Solución:
----titulo :: [String] -> [String]
----titulo [] = []
----titulo (p:ps) = mayusculaInicial p : [transforma p | p <- ps]
----donde (transforma p) es la palabra p con mayúscula inicial si su longitud es mayor o
----igual que 4 y es p en minúscula en caso contrario.
----transforma :: String -> String
----transforma p | length p >= 4 = mayusculaInicial p
--- |otherwise = minuscula p
----y (minuscula xs) es la palabra xs en minúscula.
----minuscula :: String -> String
----minuscula xs = [toLower x | x <- xs]
----Ejercicio 5.3.2. Definir, por recursión, la función
----tituloR :: [String] -> [String]
----tal que (tituloR ps) es la lista de las palabras de ps con las reglas de mayúsculas iniciales de
----los títulos. Por ejemplo,
----ghci> tituloR ["eL","arTE","DE","La","proGraMacion"]
----["El","Arte","de","la","Programacion"]
----5.4. Búsqueda en crucigramas 107
----Solución:
----tituloR :: [String] -> [String]
----tituloR [] = []
----tituloR (p:ps) = mayusculaInicial p : tituloRAux ps
----where tituloRAux [] = []
----tituloRAux (p:ps) = transforma p : tituloRAux ps
----Ejercicio 5.3.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Solución: La propiedad es
----prop_titulo :: [String] -> Bool
----prop_titulo xs = titulo xs == tituloR xs
----La comprobación es
----ghci> quickCheck prop_titulo
---- +++ OK, passed 100 tests.
----5.4. Búsqueda en crucigramas
----Ejercicio 5.4.1. Definir, por comprensión, la función
----buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
----tal que (buscaCrucigrama l pos lon ps) es la lista de las palabras de la lista de palabras ps
----que tienen longitud lon y poseen la letra l en la posición pos (comenzando en 0). Por ejemplo,
----ghci> buscaCrucigrama 'c' 1 7 ["ocaso", "acabado", "ocupado"]
----["acabado","ocupado"]
----Solución:
----buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
----buscaCrucigrama l pos lon ps =
----[p | p <- ps,
----length p == lon,
----0 <= pos, pos < length p,
----p !! pos == l]
----Ejercicio 5.4.2. Definir, por recursión, la función
----buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
----108 Capítulo 5. Funciones sobre cadenas
----tal que (buscaCrucigramaR l pos lon ps) es la lista de las palabras de la lista de palabras
----ps que tienen longitud lon y posen la letra l en la posición pos (comenzando en 0). Por ejemplo,
----ghci> buscaCrucigramaR 'c' 1 7 ["ocaso", "acabado", "ocupado"]
----["acabado","ocupado"]
----Solución:
----buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
----buscaCrucigramaR letra pos lon [] = []
----buscaCrucigramaR letra pos lon (p:ps)
--- |length p == lon && 0 <= pos && pos < length p && p !! pos == letra
---- = p : buscaCrucigramaR letra pos lon ps
--- |otherwise
---- = buscaCrucigramaR letra pos lon ps
----Ejercicio 5.4.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Solución: La propiedad es
----prop_buscaCrucigrama :: Char -> Int -> Int -> [String] -> Bool
----prop_buscaCrucigrama letra pos lon ps =
----buscaCrucigrama letra pos lon ps == buscaCrucigramaR letra pos lon ps
----La comprobación es
----ghci> quickCheck prop_buscaCrucigrama
---- +++ OK, passed 100 tests.
----5.5. Posiciones de un carácter en una cadena
----Ejercicio 5.5.1. Definir, por comprensión, la función
----posiciones :: String -> Char -> [Int]
----tal que (posiciones xs y) es la lista de la posiciones del carácter y en la cadena xs. Por
----ejemplo,
----posiciones "Salamamca" 'a' == [1,3,5,8]
----Solución:
----posiciones :: String -> Char -> [Int]
----posiciones xs y = [n | (x,n) <- zip xs [0..], x == y]
----5.6. Decidir si una cadena es subcadena de otra 109
----Ejercicio 5.5.2. Definir, por recursión, la función
----posicionesR :: String -> Char -> [Int]
----tal que (posicionesR xs y) es la lista de la posiciones del carácter y en la cadena xs. Por
----ejemplo,
----posicionesR "Salamamca" 'a' == [1,3,5,8]
----Solución:
----posicionesR :: String -> Char -> [Int]
----posicionesR xs y = posicionesAux xs y 0
----where
----posicionesAux [] y n = []
----posicionesAux (x:xs) y n | x == y = n : posicionesAux xs y (n+1)
--- |otherwise = posicionesAux xs y (n+1)
----Ejercicio 5.5.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Solución: La propiedad es
----prop_posiciones :: String -> Char -> Bool
----prop_posiciones xs y =
----posiciones xs y == posicionesR xs y
----La comprobación es
----ghci> quickCheck prop_posiciones
---- +++ OK, passed 100 tests.
----5.6. Decidir si una cadena es subcadena de otra
----Ejercicio 5.6.1. Definir, por recursión, la función
----contieneR :: String -> String -> Bool
----tal que (contieneR xs ys) se verifica si ys es una subcadena de xs. Por ejemplo,
----contieneR "escasamente" "casa" == True
----contieneR "escasamente" "cante" == False
----contieneR "" "" == True
----Nota: Se puede usar la predefinida (isPrefixOf ys xs) que se verifica si ys es un prefijo de
----xs.
----110 Capítulo 5. Funciones sobre cadenas
----Solución:
----contieneR :: String -> String -> Bool
----contieneR _ [] = True
----contieneR [] ys = False
----contieneR xs ys = isPrefixOf ys xs || contieneR (tail xs) ys
----Ejercicio 5.6.2. Definir, por comprensión, la función
----contiene :: String -> String -> Bool
----tal que (contiene xs ys) se verifica si ys es una subcadena de xs. Por ejemplo,
----contiene "escasamente" "casa" == True
----contiene "escasamente" "cante" == False
----contiene "casado y casada" "casa" == True
----contiene "" "" == True
----Nota: Se puede usar la predefinida (isPrefixOf ys xs) que se verifica si ys es un prefijo de
----xs.
----Solución:
----contiene :: String -> String -> Bool
----contiene xs ys = sufijosComenzandoCon xs ys /= []
----donde (sufijosComenzandoCon xs ys) es la lista de los sufijos de xs que comienzan
----con ys. Por ejemplo,
----sufijosComenzandoCon "abacbad" "ba" == ["bacbad","bad"]
----sufijosComenzandoCon :: String -> String -> [String]
----sufijosComenzandoCon xs ys = [x | x <- sufijos xs, isPrefixOf ys x]
----y (sufijos xs) es la lista de sufijos de xs. Por ejemplo,
----sufijos "abc" == ["abc","bc","c",""]
----sufijos :: String -> [String]
----sufijos xs = [drop i xs | i <- [0..length xs]]
----Ejercicio 5.6.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Solución: La propiedad es
----5.7. Codificación de mensajes 111
----prop_contiene :: String -> String -> Bool
----prop_contiene xs ys =
----contieneR xs ys == contiene xs ys
----La comprobación es
----ghci> quickCheck prop_contiene
---- +++ OK, passed 100 tests.
----5.7. Codificación de mensajes
----Se desea definir una función que codifique mensajes tales como
----eres lo que piensas
----del siguiente modo:
----(a) se separa la cadena en la lista de sus palabras:
----["eres","lo","que","piensas"]
----(b) se cuenta las letras de cada palabra:
----[4,2,3,7]
----(c) se une todas las palabras:
----"eresloquepiensas"
----(d) se reagrupa las letras de 4 en 4, dejando el último grupo con el resto:
----["eres","loqu","epie","nsas"]
----(e) se inverte cada palabra:
----["sere","uqol","eipe","sasn"]
----(f) se une todas las palabras:
----"sereuqoleipesasn"
----(g) se reagrupan tal como indica la inversa de la lista del apartado (b):
----["sereuqo","lei","pe","sasn"]
----(h) se crea una frase con las palabras anteriores separadas por un espacio en blanco
----112 Capítulo 5. Funciones sobre cadenas
----"sereuqo lei pe sasn"
----obteniendo así el mensaje codificado.
----En los distintos apartados de esta sección se definirá el anterior proceso de codificación.
----Ejercicio 5.7.1. Definir la función
----divide :: (a -> Bool) -> [a] -> ([a], [a])
----tal que (divide p xs) es el par (ys,zs) donde ys es el mayor prefijo de xs cuyos elementos
----cumplen p y zs es la lista de los restantes elementos de xs. Por ejemplo,
----divide (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
----divide (< 9) [1,2,3] == ([1,2,3],[])
----divide (< 0) [1,2,3] == ([],[1,2,3])
----Solución:
----divide :: (a -> Bool) -> [a] -> ([a], [a])
----divide p xs = (takeWhile p xs, dropWhile p xs)
----Es equivalente a la predefinida span
----divide' :: (a -> Bool) -> [a] -> ([a], [a])
----divide' = span
----Ejercicio 5.7.2. Definir la función
----palabras :: String -> [String]
----tal que (palabras cs) es la lista de las palabras de la cadena cs. Por ejemplo,
----palabras "eres lo que piensas" == ["eres","lo","que","piensas"]
----Solución:
----palabras :: String -> [String]
----palabras [] = []
----palabras cs = cs1 : palabras cs2
----where cs' = dropWhile (==' ') cs
----(cs1,cs2) = divide (/=' ') cs'
----Es equivalente a la predefinida words
----5.7. Codificación de mensajes 113
----palabras' :: String -> [String]
----palabras' = words
----Ejercicio 5.7.3. Definir la función
----longitudes :: [[a]] -> [Int]
----tal que (longitudes xss) es la lista de las longitudes de los elementos xss. Por ejemplo,
----longitudes ["eres","lo","que","piensas"] == [4,2,3,7]
----Solución:
----longitudes :: [[a]] -> [Int]
----longitudes = map length
----Ejercicio 5.7.4. Definir la función
----une :: [[a]] -> [a]
----tal que (une xss) es la lista obtenida uniendo los elementos de xss. Por ejemplo,
----une ["eres","lo","que","piensas"] == "eresloquepiensas"
----Solución:
----une :: [[a]] -> [a]
----une = concat
----Ejercicio 5.7.5. Definir la función
----reagrupa :: [a] -> [[a]]
----tal que (reagrupa xs) es la lista obtenida agrupando los elementos de xs de 4 en 4. Por ejemplo,
----reagrupa "eresloquepiensas" == ["eres","loqu","epie","nsas"]
----reagrupa "erestu" == ["eres","tu"]
----Solución:
----reagrupa :: [a] -> [[a]]
----reagrupa [] = []
----reagrupa xs = take 4 xs : reagrupa (drop 4 xs)
----Ejercicio 5.7.6. Definir la función
----114 Capítulo 5. Funciones sobre cadenas
----inversas :: [[a]] -> [[a]]
----tal que (inversas xss) es la lista obtenida invirtiendo los elementos de xss. Por ejemplo,
----ghci> inversas ["eres","loqu","epie","nsas"]
----["sere","uqol","eipe","sasn"]
----ghci> une (inversas ["eres","loqu","epie","nsas"])
----"sereuqoleipesasn"
----Solución:
----inversas :: [[a]] -> [[a]]
----inversas = map reverse
----Ejercicio 5.7.7. Definir la función
----agrupa :: [a] -> [Int] -> [[a]]
----tal que (agrupa xs ns) es la lista obtenida agrupando los elementos de xs según las longitudes
----indicadas en ns. Por ejemplo,
----ghci> agrupa "sereuqoleipesasn" [7,3,2,4]
----["sereuqo","lei","pe","sasn"]
----Solución:
----agrupa :: [a] -> [Int] -> [[a]]
----agrupa [] _ = []
----agrupa xs (n:ns) = (take n xs) : (agrupa (drop n xs) ns)
----Ejercicio 5.7.8. Definir la función
----frase :: [String] -> String
----tal que (frase xs) es la frase obtenida las palabras de xs dejando un espacio en blanco entre
----ellas. Por ejemplo,
----frase ["sereuqo","lei","pe","sasn"] == "sereuqo lei pe sasn"
----Solución:
----frase :: [String] -> String
----frase [x] = x
----frase (x:xs) = x ++ " " ++ frase xs
----frase [] = []
----5.8. Números de ceros finales 115
----La función frase es equivalente a unwords.
----frase' :: [String] -> String
----frase' = unwords
----Ejercicio 5.7.9. Definir la función
----clave :: String -> String
----que realice el proceso completo. Por ejemplo,
----clave "eres lo que piensas" == "sereuqo lei pe sasn"
----Solución:
----clave :: String -> String
----clave xss = frase (agrupa (une (inversas (reagrupa (une ps))))
----(reverse (longitudes ps)))
----where ps = palabras xss
----5.8. Números de ceros finales
----Ejercicio 5.8.1. Definir, por recursión, la función
----ceros :: Int -> Int
----tal que (ceros n) es el número de ceros en los que termina el número n. Por ejemplo,
----ceros 30500 == 2
----ceros 30501 == 0
----Solución:
----ceros :: Int -> Int
----ceros n | n 'rem' 10 == 0 = 1 + ceros (n 'div'10)
--- |otherwise = 0
----Ejercicio 5.8.2. Definir, sin recursión, la función
----ceros' :: Int -> Int
----tal que (ceros' n) es el número de ceros en los que termina el número n. Por ejemplo,
----ceros' 30500 == 2
----ceros' 30501 == 0
----Solución:
----ceros' :: Int -> Int
----ceros' n = length (takeWhile (=='0') (reverse (show n)))
----116 Capítulo 5. Funciones sobre
----
---- FUNCIONES DE ORDEN SUPERIOR
--6.1. Segmento inicial verificando una propiedad
--Ejercicio 6.1.1. Redefinir por recursión la función
--takeWhile :: (a -> Bool) -> [a] -> [a]
--tal que (takeWhile p xs) es la lista de los elemento de xs hasta el primero que no cumple la
--propiedad p. Por ejemplo,
--takeWhile (<7) [2,3,9,4,5] == [2,3]
--Solución:
--takeWhile' :: (a -> Bool) -> [a] -> [a]
--takeWhile' _ [] = []
--takeWhile' p (x:xs)
-- | p x = x : takeWhile' p xs
-- | otherwise = []
--6.2. Complementario del segmento inicial verificando una
--propiedad
--Ejercicio 6.2.1. Redefinir por recursión la función
--dropWhile :: (a -> Bool) -> [a] -> [a]
--tal que (dropWhile p xs) es la lista obtenida eliminando los elemento de xs hasta el primero
--que cumple la propiedad p. Por ejemplo,
--dropWhile (<7) [2,3,9,4,5] => [9,4,5]
--Solución:
--dropWhile' :: (a -> Bool) -> [a] -> [a]
--dropWhile' _ [] = []
--dropWhile' p (x:xs)
-- | p x = dropWhile' p xs
-- | otherwise = x:xs
--6.3. Concatenación de una lista de listas 119
--6.3. Concatenación de una lista de listas
--Ejercicio 6.3.1. Redefinir, por recursión, la función concat. Por ejemplo,
--concatR [[1,3],[2,4,6],[1,9]] == [1,3,2,4,6,1,9]
--Solución:
--concatR :: [[a]] -> [a]
--concatR [] = []
--concatR (xs:xss) = xs ++ concatR xss
--Ejercicio 6.3.2. Redefinir, usando foldr, la función concat. Por ejemplo,
--concatP [[1,3],[2,4,6],[1,9]] == [1,3,2,4,6,1,9]
--Solución:
--concatP :: [[a]] -> [a]
--concatP = foldr (++) []
--6.4. División de una lista numérica según su media
--Ejercicio 6.4.1. La función
--divideMedia :: [Double] -> ([Double],[Double])
--dada una lista numérica, xs, calcula el par (ys,zs), donde ys contiene los elementos de xs
--estrictamente menores que la media, mientras que zs contiene los elementos de xs estrictamente
--mayores que la media. Por ejemplo,
--divideMedia [6,7,2,8,6,3,4] == ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
--divideMedia [1,2,3] == ([1.0],[3.0])
--Definir la función divideMedia por filtrado, comprensión y recursión.
--Solución: La definición por filtrado es
--divideMediaF :: [Double] -> ([Double],[Double])
--divideMediaF xs = (filter (<m) xs, filter (>m) xs)
--where m = media xs
--donde (media xs) es la media de xs. Por ejemplo,
--120 Capítulo 6. Funciones de orden superior
--media [1,2,3] == 2.0
--media [1,-2,3.5,4] == 1.625
--media :: [Double] -> Double
--media xs = (sum xs) / fromIntegral (length xs)
--En la definición de media se usa la función fromIntegral tal que (fromIntegral x) es
--el número real correspondiente al número entero x.
--La definición por comprensión es
--divideMediaC :: [Double] -> ([Double],[Double])
--divideMediaC xs = ([x | x <- xs, x < m], [x | x <- xs, x > m])
--where m = media xs
--La definición por recursión es
--divideMediaR :: [Double] -> ([Double],[Double])
--divideMediaR xs = divideMediaR' xs
--where m = media xs
--divideMediaR' [] = ([],[])
--divideMediaR' (x:xs) | x < m = (x:ys, zs)
-- | x == m = (ys, zs)
-- | x > m = (ys, x:zs)
--where (ys, zs) = divideMediaR' xs
--Ejercicio 6.4.2. Comprobar con QuickCheck que las tres definiciones anteriores divideMediaF,
--divideMediaC y divideMediaR son equivalentes.
--Solución: La propiedad es
--prop_divideMedia :: [Double] -> Bool
--prop_divideMedia xs =
--divideMediaC xs == d &&
--divideMediaR xs == d
--where d = divideMediaF xs
--La comprobación es
--ghci> quickCheck prop_divideMedia
-- +++ OK, passed 100 tests.
--6.4. División de una lista numérica según su media 121
--Ejercicio 6.4.3. Comprobar con QuickCheck que si (ys,zs) es el par obtenido aplicándole la
--función divideMediaF a xs, entonces la suma de las longitudes de ys y zs es menor o igual que
--la longitud de xs.
--Solución: La propiedad es
--prop_longitudDivideMedia :: [Double] -> Bool
--prop_longitudDivideMedia xs =
--length ys + length zs <= length xs
--where (ys,zs) = divideMediaF xs
--La comprobación es
--ghci> quickCheck prop_longitudDivideMedia
-- +++ OK, passed 100 tests.
--Ejercicio 6.4.4. Comprobar con QuickCheck que si (ys,zs) es el par obtenido aplicándole la
--función divideMediaF a xs, entonces todos los elementos de ys son menores que todos los
--elementos de zs.
--Solución: La propiedad es
--prop_divideMediaMenores :: [Double] -> Bool
--prop_divideMediaMenores xs =
--and [y < z | y <- ys, z <- zs]
--where (ys,zs) = divideMediaF xs
--La comprobación es
--ghci> quickCheck prop_divideMediaMenores
-- +++ OK, passed 100 tests.
--Ejercicio 6.4.5. Comprobar con QuickCheck que si (ys,zs) es el par obtenido aplicándole la
--función divideMediaF a xs, entonces la media de xs no pertenece a ys ni a zs.
--Nota: Usar la función notElem tal que (notElem x ys) se verifica si y no pertenece a ys.
--Solución: La propiedad es
--prop_divideMediaSinMedia :: [Double] -> Bool
--prop_divideMediaSinMedia xs =
--notElem m (ys ++ zs)
--where m = media xs
--(ys,zs) = divideMediaF xs
--La comprobación es
--ghci> quickCheck prop_divideMediaSinMedia
-- +++ OK, passed 100 tests.
--122 Capítulo 6. Funciones de orden superior
--6.5. Segmentos cuyos elementos verifican una propiedad
--Ejercicio 6.5.1. Definir la función
--segmentos :: (a -> Bool) -> [a] -> [a]
--tal que (segmentos p xs) es la lista de los segmentos de xs cuyos elementos verifican la propiedad
--p. Por ejemplo,
--segmentos even [1,2,0,4,5,6,48,7,2] == [[],[2,0,4],[6,48],[2]]
--Solución:
--segmentos :: (a -> Bool) -> [a] -> [[a]]
--segmentos _ [] = []
--segmentos p xs =
--takeWhile p xs : (segmentos p (dropWhile (not.p) (dropWhile p xs)))
--6.6. Listas con elementos consecutivos relacionados
--Ejercicio 6.6.1. Definir la función
--relacionados :: (a -> a -> Bool) -> [a] -> Bool
--tal que (relacionados r xs) se verifica si para todo par (x,y) de elementos consecutivos de
--xs se cumple la relación r. Por ejemplo,
--relacionados (<) [2,3,7,9] == True
--relacionados (<) [2,3,1,9] == False
--relacionados equivalentes [3205,50,5014] == True
--Solución:
--relacionados :: (a -> a -> Bool) -> [a] -> Bool
--relacionados r (x:y:zs) = (r x y) && relacionados r (y:zs)
--relacionados _ _ = True
--Una definición alternativa es
--relacionados' :: (a -> a -> Bool) -> [a] -> Bool
--relacionados' r xs = and [r x y | (x,y) <- zip xs (tail xs)]
--6.7. Agrupamiento de elementos de una lista de listas 123
--6.7. Agrupamiento de elementos de una lista de listas
--Ejercicio 6.7.1. Definir la función
--agrupa :: Eq a => [[a]] -> [[a]]
--tal que (agrupa xss) es la lista de las listas obtenidas agrupando los primeros elementos, los
--segundos, . . . de forma que las longitudes de las lista del resultado sean iguales a la más corta de
--xss. Por ejemplo,
--agrupa [[1..6],[7..9],[10..20]] == [[1,7,10],[2,8,11],[3,9,12]]
--agrupa [] == []
--Solución:
--agrupa :: Eq a => [[a]] -> [[a]]
--agrupa [] = []
--agrupa xss
-- | [] 'elem' xss = []
-- | otherwise = primeros xss : agrupa (restos xss)
--where primeros = map head
--restos = map tail
--6.8. Números con dígitos pares
--Ejercicio 6.8.1. Definir, por recursión, la función
--superpar :: Int -> Bool
--tal que (superpar n) se verifica si n es un número par tal que todos sus dígitos son pares. Por
--ejemplo,
--superpar 426 == True
--superpar 456 == False
--Solución:
--superpar :: Int -> Bool
--superpar n | n < 10 = even n
-- | otherwise = even n && superpar (n 'div' 10)
--Ejercicio 6.8.2. Definir, por comprensión, la función
--superpar2 :: Int -> Bool
--124 Capítulo 6. Funciones de orden superior
--tal que (superpar2 n) se verifica si n es un número par tal que todos sus dígitos son pares. Por
--ejemplo,
--superpar2 426 == True
--superpar2 456 == False
--Solución:
--superpar2 :: Int -> Bool
--superpar2 n = and [even d | d <- digitos n]
--Donde (digitos n) es la lista de los dígitos de n.
--digitos :: Int -> [Int]
--digitos n = [read [d] | d <- show n]
--Ejercicio 6.8.3. Definir, por recursión sobre los dígitos, la función
--superpar3 :: Int -> Bool
--tal que (superpar3 n) se verifica si n es un número par tal que todos sus dígitos son pares. Por
--ejemplo,
--superpar3 426 == True
--superpar3 456 == False
--Solución:
--superpar3 :: Int -> Bool
--superpar3 n = sonPares (digitos n)
--where sonPares [] = True
--sonPares (d:ds) = even d && sonPares ds
--Ejercicio 6.8.4. Definir, usando all, la función
--superpar4 :: Int -> Bool
--tal que (superpar4 n) se verifica si n es un número par tal que todos sus dígitos son pares. Por
--ejemplo,
--superpar4 426 == True
--superpar4 456 == False
--Solución:
--6.9. Lista de los valores de los elementos que cumplen una propiedad 125
--superpar4 :: Int -> Bool
--superpar4 n = all even (digitos n)
--Ejercicio 6.8.5. Definir, usando filter, la función
--superpar5 :: Int -> Bool
--tal que (superpar5 n) se verifica si n es un número par tal que todos sus dígitos son pares. Por
--ejemplo,
--superpar5 426 == True
--superpar5 456 == False
--Solución:
--superpar5 :: Int -> Bool
--superpar5 n = filter even (digitos n) == digitos n
--6.9. Lista de los valores de los elementos que cumplen
--una propiedad
--Ejercicio 6.9.1. Se considera la función
--filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--tal que (filtraAplica f p xs) es la lista obtenida aplicándole a los elementos de xs que
--cumplen el predicado p la función f. Por ejemplo,
--filtraAplica (4+) (<3) [1..7] => [5,6]
--Se pide, definir la función
--1. por comprensión,
--2. usando map y filter,
--3. por recursión y
--4. por plegado (con foldr).
--Solución: La definición con lista de comprensión es
--filtraAplica_1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_1 f p xs = [f x | x <- xs, p x]
--126 Capítulo 6. Funciones de orden superior
--La definición con map y filter es
--filtraAplica_2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_2 f p xs = map f (filter p xs)
--La definición por recursión es
--filtraAplica_3 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_3 f p [] = []
--filtraAplica_3 f p (x:xs) | p x = f x : filtraAplica_3 f p xs
-- | otherwise = filtraAplica_3 f p xs
--La definición por plegado es
--filtraAplica_4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_4 f p = foldr g []
--where g x y | p x = f x : y
-- | otherwise = y
--La definición por plegado usando lambda es
--filtraAplica_4' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_4' f p =
--foldr (\x y -> if p x then (f x : y) else y) []
--6.10. Máximo elemento de una lista
--Ejercicio 6.10.1. Definir, mediante recursión, la función
--maximumR :: Ord a => [a] -> a
--tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--maximumR [3,7,2,5] == 7
--Nota: La función maximumR es equivalente a la predefinida maximum.
--Solución:
--maximumR :: Ord a => [a] -> a
--maximumR [x] = x
--maximumR (x:y:ys) = max x (maximumR (y:ys))
--6.11. Mínimo elemento de una lista 127
--Ejercicio 6.10.2. La función de plegado foldr1 está definida por
--foldr1 :: (a -> a -> a) -> [a] -> a
--foldr1 _ [x] = x
--foldr1 f (x:xs) = f x (foldr1 f xs)
--Definir, mediante plegado con foldr1, la función
--maximumP :: Ord a => [a] -> a
--tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--maximumP [3,7,2,5] == 7
--Nota: La función maximumP es equivalente a la predefinida maximum.
--Solución:
--maximumP :: Ord a => [a] -> a
--maximumP = foldr1 max
--6.11. Mínimo elemento de una lista
--Ejercicio 6.11.1. Definir, mediante plegado con foldr1, la función
--minimunP :: Ord a => [a] -> a
--tal que (minimunR xs) es el máximo de la lista xs. Por ejemplo,
--minimunP [3,7,2,5] == 2
--Nota: La función minimunP es equivalente a la predefinida minimun.
--Solución:
--minimumP :: Ord a => [a] -> a
--minimumP = foldr1 min
--6.12. Inversa de una lista
--Ejercicio 6.12.1. Definir, mediante recursión, la función
--inversaR :: [a] -> [a]
--tal que (inversaR xs) es la inversa de la lista xs. Por ejemplo,
--128 Capítulo 6. Funciones de orden superior
--inversaR [3,5,2,4,7] == [7,4,2,5,3]
--Solución:
--inversaR :: [a] -> [a]
--inversaR [] = []
--inversaR (x:xs) = (inversaR xs) ++ [x]
--Ejercicio 6.12.2. Definir, mediante plegado, la función
--inversaP :: [a] -> [a]
--tal que (inversaP xs) es la inversa de la lista xs. Por ejemplo,
--inversaP [3,5,2,4,7] == [7,4,2,5,3]
--Solución:
--inversaP :: [a] -> [a]
--inversaP = foldr f []
--where f x y = y ++ [x]
--La definición anterior puede simplificarse a
--inversaP_2 :: [a] -> [a]
--inversaP_2 = foldr f []
--where f x = (++ [x])
--Ejercicio 6.12.3. Definir, por recursión con acumulador, la función
--inversaR' :: [a] -> [a]
--tal que (inversaR' xs) es la inversa de la lista xs. Por ejemplo,
--inversaR' [3,5,2,4,7] == [7,4,2,5,3]
--Solución:
--inversaR' :: [a] -> [a]
--inversaR' xs = inversaAux [] xs
--where inversaAux ys [] = ys
--inversaAux ys (x:xs) = inversaAux (x:ys) xs
--Ejercicio 6.12.4. La función de plegado foldl está definida por
--6.12. Inversa de una lista 129
--foldl :: (a -> b -> a) -> a -> [b] -> a
--foldl f ys xs = aux ys xs
--where aux ys [] = ys
--aux ys (x:xs) = aux (f ys x) xs
--Definir, mediante plegado con foldl, la función
--inversaP' :: [a] -> [a]
--tal que (inversaP' xs) es la inversa de la lista xs. Por ejemplo,
--inversaP' [3,5,2,4,7] == [7,4,2,5,3]
--Solución:
--inversaP' :: [a] -> [a]
--inversaP' = foldl f []
--where f ys x = x:ys
--La definición anterior puede simplificarse lambda:
--inversaP'_2 :: [a] -> [a]
--inversaP'_2= foldl (\ys x -> x:ys) []
--La definición puede simplificarse usando flip:
--inversaP'_3 :: [a] -> [a]
--inversaP'_3 = foldl (flip(:)) []
--Ejercicio 6.12.5. Comprobar con QuickCheck que las funciones reverse, inversaP e inversaP'
--son equivalentes.
--Solución: La propiedad es
--prop_inversa :: Eq a => [a] -> Bool
--prop_inversa xs =
--inversaP xs == ys &&
--inversaP' xs == ys
--where ys = reverse xs
--La comprobación es
--ghci> quickCheck prop_inversa
-- +++ OK, passed 100 tests.
--130 Capítulo 6. Funciones de orden superior
--Ejercicio 6.12.6. Comparar la eficiencia de inversaP e inversaP' calculando el tiempo y el
--espacio que usado en evaluar las siguientes expresiones:
--head (inversaP [1..100000])
--head (inversaP' [1..100000])
--Solución: La sesión es
--ghci> :set +s
--ghci> head (inversaP [1..100000])
--100000
--(0.41 secs, 20882460 bytes)
--ghci> head (inversaP' [1..100000])
--1
--(0.00 secs, 525148 bytes)
--ghci> :unset +s
--6.13. Número correspondiente a la lista de sus cifras
--Ejercicio 6.13.1. Definir, por recursión con acumulador, la función
--dec2entR :: [Int] -> Int
--tal que (dec2entR xs) es el entero correspondiente a la expresión decimal xs. Por ejemplo,
--dec2entR [2,3,4,5] == 2345
--Solución:
--dec2entR :: [Int] -> Int
--dec2entR xs = dec2entR' 0 xs
--where dec2entR' a [] = a
--dec2entR' a (x:xs) = dec2entR' (10*a+x) xs
--Ejercicio 6.13.2. Definir, por plegado con foldl, la función
--dec2entP :: [Int] -> Int
--tal que (dec2entP xs) es el entero correspondiente a la expresión decimal xs. Por ejemplo,
--dec2entP [2,3,4,5] == 2345
--Solución:
--6.14. Suma de valores de una aplicación a una lista 131
--dec2entP :: [Int] -> Int
--dec2entP = foldl f 0
--where f a x = 10*a+x
--La definición puede simplificarse usando lambda:
--dec2entP' :: [Int] -> Int
--dec2entP' = foldl (\a x -> 10*a+x) 0
--6.14. Suma de valores de una aplicación a una lista
--Ejercicio 6.14.1. Definir, por recursión, la función
--sumaR :: Num b => (a -> b) -> [a] -> b
--tal que (suma f xs) es la suma de los valores obtenido aplicando la función f a lo elementos de
--la lista xs. Por ejemplo,
--sumaR (*2) [3,5,10] == 36
--sumaR (/10) [3,5,10] == 1.8
--Solución:
--sumaR :: Num b => (a -> b) -> [a] -> b
--sumaR f [] = 0
--sumaR f (x:xs) = f x + sumaR f xs
--Ejercicio 6.14.2. Definir, por plegado, la función
--sumaP :: Num b => (a -> b) -> [a] -> b
--tal que (suma f xs) es la suma de los valores obtenido aplicando la función f a lo elementos de
--la lista xs. Por ejemplo,
--sumaP (*2) [3,5,10] == 36
--sumaP (/10) [3,5,10] == 1.8
--Solución:
--sumaP :: Num b => (a -> b) -> [a] -> b
--sumaP f = foldr (\x y -> (f x) + y) 0
--132 Capítulo 6. Funciones de orden superior
--6.15. Redefinición de la función map usando foldr
--Ejercicio 6.15.1. Redefinir, por recursión, la función map. Por ejemplo,
--mapR (+2) [1,7,3] == [3,9,5]
--Solución:
--mapR :: (a -> b) -> [a] -> [b]
--mapR f [] = []
--mapR f (x:xs) = f x : mapR f xs
--Ejercicio 6.15.2. Redefinir, usando foldr, la función map. Por ejemplo,
--mapP (+2) [1,7,3] == [3,9,5]
-

--Solución:
--mapP :: (a -> b) -> [a] -> [b]
--mapP f = foldr g []
--where g x xs = f x : xs
--La definición por plegado usando lambda es
--mapP' :: (a -> b) -> [a] -> [b]
--mapP' f = foldr (\x y -> f x:y) []
--Otra definición es
--mapP'' :: (a -> b) -> [a] -> [b]
--mapP'' f = foldr ((:) . f) []
--6.16. Redefinición de la función filter usando foldr
--Ejercicio 6.16.1. Redefinir, por recursión, la función filter. Por ejemplo,
--filterR (<4) [1,7,3,2] => [1,3,2]
--Solución:
--filterR :: (a -> Bool) -> [a] -> [a]
--filterR p [] = []
--filterR p (x:xs) | p x = x : filterR p xs
-- | otherwise = filterR p xs
--6.17. Suma de las sumas de las listas de una lista de listas 133
--Ejercicio 6.16.2. Redefinir, usando foldr, la función filter. Por ejemplo,
--filterP (<4) [1,7,3,2] => [1,3,2]
--Solución:
--filterP :: (a -> Bool) -> [a] -> [a]
--filterP p = foldr g []
--where g x y | p x = x:y
-- | otherwise = y
--La definición por plegado y lambda es
--filterP' :: (a -> Bool) -> [a] -> [a]
--filterP' p = foldr (\x y -> if (p x) then (x:y) else y) []
--6.17. Suma de las sumas de las listas de una lista de listas
--Ejercicio 6.17.1. Definir, mediante recursión, la función
--sumllR :: Num a => [[a]] -> a
--tal que (sumllR xss) es la suma de las sumas de las listas de xss. Por ejemplo,
--sumllR [[1,3],[2,5]] == 11
--Solución:
--sumllR :: Num a => [[a]] -> a
--sumllR [] = 0
--sumllR (xs:xss) = sum xs + sumllR xss
--Ejercicio 6.17.2. Definir, mediante plegado, la función
--sumllP :: Num a => [[a]] -> a
--tal que (sumllP xss) es la suma de las sumas de las listas de xss. Por ejemplo,
--sumllP [[1,3],[2,5]] == 11
--Solución:
--sumllP :: Num a => [[a]] -> a
--sumllP = foldr f 0
--where f xs n = sum xs + n
--134 Capítulo 6. Funciones de orden superior
--La definición anterior puede simplificarse usando lambda
--sumllP' :: Num a => [[a]] -> a
--sumllP' = foldr (\xs n -> sum xs + n) 0
--Ejercicio 6.17.3. Definir, mediante recursión con acumulador, la función
--sumllA :: Num a => [[a]] -> a
--tal que (sumllA xss) es la suma de las sumas de las listas de xss. Por ejemplo,
--sumllA [[1,3],[2,5]] == 11
--Solución:
--sumllA :: Num a => [[a]] -> a
--sumllA xs = aux 0 xs
--where aux a [] = a
--aux a (xs:xss) = aux (a + sum xs) xss
--Ejercicio 6.17.4. Definir, mediante plegado con foldl, la función
--sumllAP :: Num a => [[a]] -> a
--tal que (sumllAP xss) es la suma de las sumas de las listas de xss. Por ejemplo,
--sumllAP [[1,3],[2,5]] == 11
--Solución:
--sumllAP :: Num a => [[a]] -> a
--sumllAP = foldl (\a xs -> a + sum xs) 0
--6.18. Lista obtenida borrando las ocurrencias de un elemento
--Ejercicio 6.18.1. Definir, mediante recursión, la función
--borraR :: Eq a => a -> a -> [a]
--tal que (borraR y xs) es la lista obtenida borrando las ocurrencias de y en xs. Por ejemplo,
--borraR 5 [2,3,5,6] == [2,3,6]
--borraR 5 [2,3,5,6,5] == [2,3,6]
--borraR 7 [2,3,5,6,5] == [2,3,5,6,5]
--6.19. Diferencia de dos listas 135
--Solución:
--borraR :: Eq a => a -> [a] -> [a]
--borraR z [] = []
--borraR z (x:xs) | z == x = borraR z xs
-- | otherwise = x : borraR z xs
--Ejercicio 6.18.2. Definir, mediante plegado, la función
--borraP :: Eq a => a -> a -> [a]
--tal que (borraP y xs) es la lista obtenida borrando las ocurrencias de y en xs. Por ejemplo,
--borraP 5 [2,3,5,6] == [2,3,6]
--borraP 5 [2,3,5,6,5] == [2,3,6]
--borraP 7 [2,3,5,6,5] == [2,3,5,6,5]
--Solución:
--borraP :: Eq a => a -> [a] -> [a]
--borraP z = foldr f []
--where f x y | z == x = y
-- | otherwise = x:y
--La definición por plegado con lambda es es
--borraP' :: Eq a => a -> [a] -> [a]
--borraP' z = foldr (\x y -> if z==x then y else x:y) []
--6.19. Diferencia de dos listas
--Ejercicio 6.19.1. Definir, mediante recursión, la función
--diferenciaR :: Eq a => [a] -> [a] -> [a]
--tal que (diferenciaR xs ys) es la diferencia del conjunto xs e ys; es decir el conjunto de los
--elementos de xs que no pertenecen a ys. Por ejemplo,
--diferenciaR [2,3,5,6] [5,2,7] == [3,6]
--Solución:
--136 Capítulo 6. Funciones de orden superior
--diferenciaR :: Eq a => [a] -> [a] -> [a]
--diferenciaR xs ys = aux xs xs ys
--where aux a xs [] = a
--aux a xs (y:ys) = aux (borraR y a) xs ys
--La definición, para aproximarse al patrón de plegado, se puede escribir como
--diferenciaR' :: Eq a => [a] -> [a] -> [a]
--diferenciaR' xs ys = aux xs xs ys
--where aux a xs [] = a
--aux a xs (y:ys) = aux (flip borraR a y) xs ys
--Ejercicio 6.19.2. Definir, mediante plegado con foldl, la función
--diferenciaP :: Eq a => [a] -> [a] -> [a]
--tal que (diferenciaP xs ys) es la diferencia del conjunto xs e ys; es decir el conjunto de los
--elementos de xs que no pertenecen a ys. Por ejemplo,
--diferenciaP [2,3,5,6] [5,2,7] == [3,6]
--Solución:
--diferenciaP :: Eq a => [a] -> [a] -> [a]
--diferenciaP xs ys = foldl (flip borraR) xs ys
--La definición anterior puede simplificarse a
--diferenciaP' :: Eq a => [a] -> [a] -> [a]
--diferenciaP' = foldl (flip borraR)
--6.20. Producto de los números que verifican una propiedad
--Ejercicio 6.20.1. Definir mediante plegado la función
--producto :: Num a => [a] -> a
--tal que (producto xs) es el producto de los elementos de la lista xs. Por ejemplo,
--producto [2,1,-3,4,5,-6] == 720
--6.21. Las cabezas y las colas de una lista 137
--Solución:
--producto :: Num a => [a] -> a
--producto = foldr (*) 1
--Ejercicio 6.20.2. Definir mediante plegado la función
--productoPred :: Num a => (a -> Bool) -> [a] -> a
--tal que (productoPred p xs) es el producto de los elementos de la lista xs que verifican el
--predicado p. Por ejemplo,
--productoPred even [2,1,-3,4,-5,6] == 48
--Solución:
--productoPred :: Num a => (a -> Bool) -> [a] -> a
--productoPred p = foldr (\x y -> if p x then x*y else y) 1
--Ejercicio 6.20.3. Definir la función
--productoPos :: (Num a, Ord a) => [a] -> a
--tal que (productoPos xs) esel producto de los elementos estríctamente positivos de la lista xs.
--Por ejemplo,
--productoPos [2,1,-3,4,-5,6] == 48
--Solución:
--productoPos :: (Num a, Ord a) => [a] -> a
--productoPos = productoPred (>0)
--6.21. Las cabezas y las colas de una lista
--Ejercicio 6.21.1. Se denomina cola de una lista xs a una sublista no vacía de xs formada
--por un elemento y los siguientes hasta el final. Por ejemplo, [3,4,5] es una cola de la lista
--[1,2,3,4,5].
--Definir la función
--colas :: [a] -> [[a]]
--tal que (colas xs) es la lista de las colas de la lista xs. Por ejemplo,
--138 Capítulo 6. Funciones de orden superior
--colas [] == [[]]
--colas [1,2] == [[1,2],[2],[]]
--colas [4,1,2,5] == [[4,1,2,5],[1,2,5],[2,5],[5],[]]
--Solución:
--colas :: [a] -> [[a]]
--colas [] = [[]]
--colas (x:xs) = (x:xs) : colas xs
--Ejercicio 6.21.2. Comprobar con QuickCheck que las funciones colas y tails son equivalentes.
--Solución: La propiedad es
--prop_colas :: [Int] -> Bool
--prop_colas xs = colas xs == tails xs
--La comprobación es
--ghci> quickCheck prop_colas
-- +++ OK, passed 100 tests.
--Ejercicio 6.21.3. Se denomina cabeza de una lista xs a una sublista no vacía de xs formada
--por el primer elemento y los siguientes hasta uno dado. Por ejemplo, [1,2,3] es una cabeza de
--[1,2,3,4,5].
--Definir, por recursión, la función
--cabezas :: [a] -> [[a]]
--tal que (cabezas xs) es la lista de las cabezas de la lista xs. Por ejemplo,
--cabezas [] == [[]]
--cabezas [1,4] == [[],[1],[1,4]]
--cabezas [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]
--Solución:
--cabezas :: [a] -> [[a]]
--cabezas [] = [[]]
--cabezas (x:xs) = [] : [x:ys | ys <- cabezas xs]
--Ejercicio 6.21.4. Definir, por plegado, la función
--cabezasP :: [a] -> [[a]]
--6.21. Las cabezas y las colas de una lista 139
--tal que (cabezasP xs) es la lista de las cabezasP de la lista xs. Por ejemplo,
--cabezasP [] == [[]]
--cabezasP [1,4] == [[],[1],[1,4]]
--cabezasP [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]
--Solución:
--cabezasP :: [a] -> [[a]]
--cabezasP = foldr (\x y -> [x]:[x:ys | ys <- y]) []
--Ejercicio 6.21.5. Definir, mediante funciones de orden superior, la función
--cabezasS :: [a] -> [[a]]
--tal que (cabezasS xs) es la lista de las cabezasS de la lista xs. Por ejemplo,
--cabezasS [] == [[]]
--cabezasS [1,4] == [[],[1],[1,4]]
--cabezasS [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]
--Solución:
--cabezasS :: [a] -> [[a]]
--cabezasS xs = reverse (map reverse (colas (reverse xs)))
--La anterior definición puede escribirse sin argumentos como
--cabezasS' :: [a] -> [[a]]
--cabezasS' = reverse . map reverse . (colas . reverse)
--Ejercicio 6.21.6. Comprobar con QuickCheck que las funciones cabezas y inits son equivalentes.
--Solución: La propiedad es
--prop_cabezas :: [Int] -> Bool
--prop_cabezas xs = cabezas xs == inits xs
--La comprobación es
--ghci> quickCheck prop_cabezas
-- +++ OK, passed 100 tests.
-- Nota. Un caso de estudio para las funciones de orden superior es el capítulo 16 “Codificación
--  y transmisión de mensajes” (página 331)
