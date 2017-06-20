module PiensaEnHaskell.PiensaEnHaskell where



----Ejercicio 1.22.1. Las dimensiones de los rect�ngulos puede representarse por pares; por ejemplo,
----(5,3) representa a un rect�ngulo de base 5 y altura 3. Definir la funci�n mayorRectangulo tal
----que (mayorRectangulo r1 r2) es el rect�ngulo de mayor �rea entre r1 y r2. Por ejemplo,
----mayorRectangulo (4,6) (3,7) == (4,6)
----mayorRectangulo (4,6) (3,8) == (4,6)
----mayorRectangulo (4,6) (3,9) == (3,9)

mayorRectangulo::(Int,Int)->(Int,Int)->(Int,Int)
mayorRectangulo (x,y)(z,t)= if (compara) then (x,y) else (z,t)
	where
		compara = x*y>z*t

--DEFINICIONE POR COMPRESION

-- 2.1. Suma de los cuadrados de los n primeros n�meros
-- es la suma de los cuadrados de los primeros n n�meros; es decir, 12 + 22 +    + n2. 
-- sumaDeCuadrados 3 == 14
-- sumaDeCuadrados 100 == 338350

sumaDeCuadrados:: Int->Int
sumaDeCuadrados x= sum [n^2|n<-[1..x]]


-- Ejercicio 2.2.1. Definir por comprensi�n la funci�n
-- tal que (replica n x) es la lista formada por n copias del elemento x. Por ejemplo,
-- replica 3 True == [True, True, True]
-- Nota: La funci�n replica es equivalente a la predefinida replicate.
--IMPORTANTE
replica :: Int -> a -> [a]
replica x y= [y|_<-[1..x]]



----Ejercicio 2.3.1. Definir la funci�n suma tal (suma n) es la suma de los n primeros n�meros.
----Por ejemplo,
----suma 3 == 6

suma:: Int->Int
suma x= sum [x|x<-[1..x]]




----Definir la funci�n linea tal que (linea n) es la l�nea n��sima de los tri�ngulos aritm�ticos.
----Por ejemplo,
----linea 4 == [7,8,9,10]
----linea 5 == [11,12,13,14,15]

linea::Int->[Int]
linea x= [n|n<-[suma(x-1)+1..suma(x)]]


----Ejercicio 2.3.3. Definir la funci�n triangulo tal que (triangulo n) es el tri�ngulo aritm�tico
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

----2.4. N�meros perfectos
----Ejercicio 2.4.1. Un entero positivo es perfecto si es igual a la suma de sus factores, excluyendo
----el propio n�mero. Definir por comprensi�n la funci�n
----perfectos :: Int -> [Int]

----42 Cap�tulo 2. Definiciones por comprensi�n
----tal que (perfectos n) es la lista de todos los n�meros perfectos menores que n. Por ejemplo,
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



----2.5. N�meros abundantes
----Un n�mero natural n se denomina abundante si es menor que la suma de sus divisores
----propios. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.
----Ejercicio 2.5.1. Definir la funci�n numeroAbundante tal que (numeroAbundante n) se verifica
----si n es un n�mero abundante. Por ejemplo,
----numeroAbundante 5 == False
----numeroAbundante 12 == True
----numeroAbundante 28 == False
----numeroAbundante 30 == True

--version 1 
numeroAbundante::Int->Bool
numeroAbundante x = x > (sum[n|n<-[1..x-1],x `mod` n == 0])

----Ejercicio 2.5.2. Definir la funci�n numerosAbundantesMenores tal que (numerosAbundantesMenores n)
----es la lista de n�meros abundantes menores o iguales que n. Por ejemplo,
----numerosAbundantesMenores 50 == [12,18,20,24,30,36,40,42,48]
numerosAbundantesMenores::Int->[Int]
numerosAbundantesMenores x=[n|n<-[1..x-1],x `mod` n == 0]

----2.6. Problema 1 del proyecto Euler 43
----Ejercicio 2.5.3. Definir la funci�n todosPares tal que (todosPares n) se verifica si todos los
----n�meros abundantes menores o iguales que n son pares. Por ejemplo,
----todosPares 10 == True
----todosPares 100 == True
----todosPares 1000 == False

todosPares::Int->Bool
todosPares x=and[even n|n <- numerosAbundantesMenores x]

----Ejercicio 2.5.4. Definir la constante primerAbundanteImpar que calcule el primer n�mero
----natural abundante impar. Determinar el valor de dicho n�mero.
----Su c�lculo es
----primerAbundanteImpar----945
primerAbundanteImpar :: Int	
primerAbundanteImpar = head [x | x <-[1..], numeroAbundante x, odd x]

----Ejercicio 2.6.1. Definir la funci�n
----euler1 :: Integer -> Integer
----tal que (euler1 n) es la suma de todos los m�ltiplos de 3 � 5 menores que n. Por ejemplo,
----euler1 10 == 23
-- version1
euler::Int->Int
euler x = sum[n|n<-[1..x-1], x`mod`5==0||x`mod`3==0 ]

--version2
euler1::Int->Int
euler1 x=sum [n|n<-[1..x-1],multiplo x 3|| multiplo x 5]
	where multiplo x y = mod x y == 0



----Definir la funci�n aproxE' tal que (aproxE' n) es la aproximaci�n de e que se obtiene sumando
----los t�rminos de la serie hasta 1
----n! . Por ejemplo,
----aproxE' 10 == 2.718281801146385
----aproxE' 100 == 2.7182818284590455
-- aproxE:: Int->[Int]
-- aproxE n = [(1+1/m)**m | m <- [1..n]]



----2.11. Ternas pitag�ricas
----Ejercicio 2.11.1. Una terna (x, y, z) de enteros positivos es pitag�rica si x2 + y2 = z2. Usando
----una lista por comprensi�n, definir la funci�n
----
----tal que (pitagoricas n) es la lista de todas las ternas pitag�ricas cuyas componentes est�n
----entre 1 y n. Por ejemplo,
----pitagoricas 10 == [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
--pitagoricas :: Int -> [(Int,Int,Int)]



pitagoricas :: Int -> [(Int,Int,Int)]
pitagoricas n = [(x,y,z) | x <- [1..n],y <- [1..n],z <- [1..n], x^2 + y^2 == z^2]





----Ejercicio 2.11.2. Definir la funci�n
----numeroDePares :: (Int,Int,Int) -> Int
----tal que (numeroDePares t) es el n�mero de elementos pares de la terna t. Por ejemplo,
----48 Cap�tulo 2. Definiciones por comprensi�n
----numeroDePares (3,5,7) == 0
----numeroDePares (3,6,7) == 1
----numeroDePares (3,6,4) == 2
----numeroDePares (4,6,4) == 3

numeroDePares::(Int,Int,Int)->Int
numeroDePares(x,y,z)= sum[1|n<-[x,y,z],even n]


----Ejercicio 2.11.3. Definir la funci�n
----conjetura :: Int -> Bool
----tal que (conjetura n) se verifica si todas las ternas pitag�ricas cuyas componentes est�n entre
----1 y n tiene un n�mero impar de n�meros pares. Por ejemplo,
----conjetura 10 == True

conjetura :: Int -> Bool
conjetura n = and [odd (numeroDePares t) | t <- pitagoricas n]







----2.12. Problema 9 del Proyecto Euler
----Ejercicio 2.12.1. Una terna pitag�rica es una terna de n�meros naturales (a, b, c) tal que a <
----b < c y a2 + b2 = c2. Por ejemplo (3, 4, 5) es una terna pitag�rica. Definir la funci�n
----ternasPitagoricas :: Integer -> [[Integer]]
----tal que (ternasPitagoricas x) es la lista de las ternas pitag�ricas cuya suma es x. Por
----ternasPitagoricas 12 == [(3,4,5)]
----ternasPitagoricas 60 == [(10,24,26),(15,20,25)]

ternasPitagoricas :: Integer -> [(Integer,Integer,Integer)]
ternasPitagoricas x = [(a,b,c) | a <- [1..x],
								 b <- [a+1..x],
								 c <- [x-a-b],
								 a^2 + b^2 == c^2]


----Ejercicio 2.12.2. Definir la constante euler9 tal que euler9 es producto abc donde (a, b, c) es
----la �nica terna pitag�rica tal que a + b + c = 1000. Calcular el valor de euler9.
----Soluci�n:
----euler9 = a*b*c
----where (a,b,c) = head (ternasPitagoricas 1000)
----El c�lculo del valor de euler9 es


----2.13. Producto escalar
----Ejercicio 2.13.1. El producto escalar de dos listas de enteros xs e ys de longitud n viene dado por
----la suma de los productos de los elementos correspondientes. Definir por comprensi�n la funci�n
----productoEscalar :: [Int] -> [Int] -> Int
----tal que (productoEscalar xs ys) es el producto escalar de las listas xs e ys. Por ejemplo,
----productoEscalar [1,2,3] [4,5,6] == 32

productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys = sum [x*y | (x,y) <- zip xs ys]



----2.14. Suma de pares de elementos consecutivos
----Ejercicio 2.14.1. Definir, por comprensi�n, la funci�n
----sumaConsecutivos :: [Int] -> [Int]
----tal que (sumaConsecutivos xs) es la suma de los pares de elementos consecutivos de la lista
----xs. Por ejemplo,
----sumaConsecutivos [3,1,5,2] == [4,6,7]
----sumaConsecutivos [3] == []

sumaConsecutivos::[Int]->[Int]
sumaConsecutivos xs =[x+y|(x,y)<- zip xs(tail xs)]





----2.15. Posiciones de un elemento en una lista
----Ejercicio 2.15.1. En el tema se ha definido la funci�n
----posiciones :: Eq a => a -> [a] -> [Int]
----tal que (posiciones x xs) es la lista de las posiciones ocupadas por el elemento x en la lista
----xs. Por ejemplo,
----posiciones 5 [1,5,3,5,5,7] == [1,3,4]
----Definir, usando la funci�n busca (definida en el tema 5), la funci�n
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

----Soluci�n: La definici�n de posiciones es
----posiciones :: Eq a => a -> [a] -> [Int]
----posiciones x xs =
----[i | (x',i) <- zip xs [0..n], x == x']
----where n = length xs - 1
----La definici�n de busca es
----busca :: Eq a => a -> [(a, b)] -> [b]
----busca c t = [v | (c', v) <- t, c' == c]


----La redefinici�n de posiciones es
----posiciones' :: Eq a => a -> [a] -> [Int]
----posiciones' x xs = busca x (zip xs [0..])




----2.16. Representaci�n densa de un polinomio representado dispersamente 51
----2.16. Representaci�n densa de un polinomio representado
----dispersamente
----Ejercicio 2.16.1. Los polinomios pueden representarse de forma dispersa o densa. Por ejemplo,
----el polinomio 6x4 ?? 5x2 + 4x ?? 7 se puede representar de forma dispersa por [6,0,-5,4,-7] y
----de forma densa por [(4,6),(2,-5),(1,4),(0,-7)]. Definir la funci�n
----densa :: [Int] -> [(Int,Int)]
----tal que (densa xs) es la representaci�n densa del polinomio cuya representaci�n dispersa es xs.
----Por ejemplo,
----densa [6,0,-5,4,-7] == [(4,6),(2,-5),(1,4),(0,-7)]
----densa [6,0,0,3,0,4] == [(5,6),(2,3),(0,4)]

densa :: [Int] -> [(Int,Int)]
densa xs = [(x,y) | (x,y) <- zip [n-1,n-2..0] xs, y /= 0]
	where n = length xs


----2.17. Producto cartesiano
----Ejercicio 2.17.1. La funci�n
----pares :: [a] -> [b] -> [(a,b)]
----definida por
----pares xs ys = [(x,y) | x <- xs, y <- ys]
----toma como argumento dos listas y devuelve la listas de los pares con el primer elemento de la
----primera lista y el segundo de la segunda. Por ejemplo,
----ghci> pares [1..3] [4..6]
----[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
----Definir, usando dos listas por comprensi�n con un generador cada una, la funci�n
----pares' :: [a] -> [b] -> [(a,b)]
----tal que pares' sea equivalente a pares.
----Indicaci�n: Utilizar la funci�n predefinida concat y encajar una lista por comprensi�n
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


----Ejercicio 2.18.1. Definir la funci�n nombres tal que (nombres bd) es la lista de los nombres
----de las personas de la base de datos bd. Por ejemplo,
----ghci> nombres personas["Cervantes","Velazquez","Picasso","Beethoven","Poincare","Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini","Bach"]

nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [x | (x,_,_,_) <- bd]


----Ejercicio 2.18.2. Definir la funci�n musicos tal que (musicos bd) es la lista de los nombres
----de los m�sicos de la base de datos bd. Por ejemplo,
----ghci> musicos personas
----["Beethoven","Mozart","Bach"]

musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [x | (x,m,_,_) <- bd, m == "Musica"]
----
----2.18. Consulta de bases de datos 53
----Ejercicio 2.18.3. Definir la funci�n seleccion tal que (seleccion bd m) es la lista de los
----nombres de las personas de la base de datos bd cuya actividad es m. Por ejemplo,
----ghci> seleccion personas "Pintura"
----["Velazquez","Picasso","Goya","Botticelli"]
----Soluci�n:
----seleccion :: [(String,String,Int,Int)] -> String -> [String]
----seleccion bd m = [ x | (x,m',_,_) <- bd, m == m' ]
----Ejercicio 2.18.4. Definir, usando el apartado anterior, la funci�n musicos' tal que (musicos' bd)
----es la lista de los nombres de los m�sicos de la base de datos bd. Por ejemplo,
----ghci> musicos' personas
----["Beethoven","Mozart","Bach"]

--musicos' :: [(String,String,Int,Int)] -> [String]
--musicos' bd = seleccion bd "Musica"
----Ejercicio 2.18.5. Definir la funci�n vivas tal que (vivas bd a) es la lista de los nombres de
----las personas de la base de datos bd que estaban vivas en el a�o a. Por ejemplo,
----ghci> vivas personas 1600
----["Cervantes","Velazquez","Quevedo","Borromini"]

vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas ps a = [x | (x,_,a1,a2) <- ps, a1 <= a, a <= a2]
----Nota. Un caso de estudio para las definiciones por comprensi�n es el cap�tulo 15 �El
----cifrado C�sar� (p�gina 325)




----------- definidiones por comprension
----
----3.1. Potencia de exponente natural
----Ejercicio 3.1.1. Definir por recursi�n la funci�n
----potencia :: Integer -> Integer -> Integer
----tal que (potencia x n) es x elevado al n�mero natural n. Por ejemplo,
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



----3.2. Replicaci�n de un elemento
----Ejercicio 3.2.1. Definir por recursi�n la funci�n
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

-- MARTES MA�ANA

----3.3. Doble factorial 57
----3.3. Doble factorial
----Ejercicio 3.3.1. El doble factorial de un n�mero n se define por
----0!! = 1
----1!! = 1
----n!! = n*(n-2)* ... * 3 * 1, si n es impar
----n!! = n*(n-2)* ... * 4 * 2, si n es par
----Por ejemplo,
----8!! = 8*6*4*2 = 384
----9!! = 9*7*5*3*1 = 945
----Definir, por recursi�n, la funci�n
----dobleFactorial :: Integer -> Integer
----tal que (dobleFactorial n) es el doble factorial de n. Por ejemplo,
----dobleFactorial 8 == 384
----dobleFactorial 9 == 945



----3.5. Menor n�mero divisible por una sucesi�n de n�meros
----Los siguientes ejercicios tienen como objetivo resolver el problema 5 del proyecto
----Euler que consiste en calcular el menor n�mero divisible por los n�meros del 1 al 20.
----Ejercicio 3.5.1. Definir por recursi�n la funci�n
----menorDivisible :: Integer -> Integer -> Integer
----tal que (menorDivisible a b) es el menor n�mero divisible por los n�meros del a al b. Por
----ejemplo,
----menorDivisible 2 5 == 60
----Indicaci�n: Usar la funci�n lcm tal que (lcm x y) es el m�nimo com�n m�ltiplo de x e y.
----Soluci�n:
----menorDivisible :: Integer -> Integer -> Integer
----menorDivisible a b
--- | a == b = a
--- | otherwise = lcm a (menorDivisible (a+1) b)
----Ejercicio 3.5.2. Definir la constante
----euler5 :: Integer
----tal que euler5 es el menor n�mero divisible por los n�meros del 1 al 20 y calcular su valor.
----Soluci�n:
----euler5 :: Integer
----euler5 = menorDivisible 1 20
----El c�lculo es
----ghci> euler5
----232792560




----3.9. �ltimo elemento de una lista
----Ejercicio 3.9.1. Definir por recursi�n la funci�n
----last' :: [a] -> a
----tal que (last xs) es el �ltimo elemento de xs. Por ejemplo,
----last' [2,3,5] => 5
----Soluci�n:
----last' :: [a] -> a
----last' [x] = x
----last' (_:xs) = last' xs
----3.10. Concatenaci�n de una lista 61
----3.10. Concatenaci�n de una lista
----Ejercicio 3.10.1. Definir por recursi�n la funci�n
----concat' :: [[a]] -> [a]
----tal que (concat' xss) es la lista obtenida concatenando las listas de xss. Por ejemplo,
----concat' [[1..3],[5..7],[8..10]] == [1,2,3,5,6,7,8,9,10]

----3.11. Selecci�n de un elemento
----Ejercicio 3.11.1. Definir por recursi�n la funci�n
----selecciona :: [a] -> Int -> a
----tal que (selecciona xs n) es el n��simo elemento de xs. Por ejemplo,
----selecciona [2,3,5,7] 2 == 5


----3.12. Selecci�n de los primeros elementos
----Ejercicio 3.12.1. Definir por recursi�n la funci�n



----take' :: Int -> [a] -> [a]
----tal que (take' n xs) es la lista de los n primeros elementos de xs. Por ejemplo,
----take' 3 [4..12] == [4,5,6]
----Soluci�n:
----take' :: Int -> [a] -> [a]
----take' 0 _ = []
----take' _ [] = []
----take' n (x:xs) = x : take' (n-1) xs
----62 Cap�tulo 3. Definiciones por recursi�n
----3.13. Intercalaci�n de la media aritm�tica
----Ejercicio 3.13.1. Definir la funci�n
----refinada :: [Float] -> [Float]
----tal que (refinada xs) es la lista obtenida intercalando entre cada dos elementos consecutivos
----de xs su media aritm�tica. Por ejemplo,
----refinada [2,7,1,8] == [2.0,4.5,7.0,4.0,1.0,4.5,8.0]
----refinada [2] == [2.0]
----refinada [] == []

----3.14.5. La ordenaci�n por mezcla da una permutaci�n
----Ejercicio 3.14.6. Definir por recursi�n la funci�n
----borra :: Eq a => a -> [a] -> [a]
----tal que (borra x xs) es la lista obtenida borrando una ocurrencia de x en la lista xs. Por
----ejemplo,
----borra 1 [1,2,1] == [2,1]
----borra 3 [1,2,1] == [1,2,1]
----Soluci�n:
----borra :: Eq a => a -> [a] -> [a]
----borra x [] = []
----borra x (y:ys) | x == y = ys
--- |otherwise = y : borra x ys
----3.14. Ordenaci�n por mezcla 65
----3.14.6. Determinaci�n de permutaciones
----Ejercicio 3.14.7. Definir por recursi�n la funci�n
----esPermutacion :: Eq a => [a] -> [a] -> Bool
----tal que (esPermutacion xs ys) se verifica si xs es una permutaci�n de ys. Por ejemplo,
----esPermutacion [1,2,1] [2,1,1] == True
----esPermutacion [1,2,1] [1,2,2] == False
----Soluci�n:
----esPermutacion :: Eq a => [a] -> [a] -> Bool
----esPermutacion [] [] = True
----esPermutacion [] (y:ys) = False
----esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (borra x ys)
----Ejercicio 3.14.8. Comprobar con QuickCheck que la ordenaci�n por mezcla de una lista es una
----permutaci�n de la lista.
----Soluci�n: La propiedad es
----prop_ordMezcla_pemutacion :: Ord a => [a] -> Bool
----prop_ordMezcla_pemutacion xs = esPermutacion (ordMezcla xs) xs
----La comprobaci�n es
----ghci> quickCheck prop_ordMezcla_permutacion
---- +++ OK, passed 100 tests.
----


----------- comprension y recursion
----
----4.1. Suma de los cuadrados de los primeros n�meros
----Ejercicio 4.1.1. Definir, por recursi�n; la funci�n
----4.1. Suma de los cuadrados de los primeros n�meros 69
----sumaCuadradosR :: Integer -> Integer
----tal que (sumaCuadradosR n) es la suma de los cuadrados de los n�meros de 1 a n. Por ejemplo,
----sumaCuadradosR 4 == 30
----Soluci�n:
----sumaCuadradosR :: Integer -> Integer
----sumaCuadradosR 0 = 0
----sumaCuadradosR n = n^2 + sumaCuadradosR (n-1)
----Ejercicio 4.1.2. Comprobar con QuickCheck si (sumaCuadradosR n) es igual a n(n+1)(2n+1)
----6 .
----Soluci�n: La propiedad es
----prop_SumaCuadrados n =
----n >= 0 ==>
----sumaCuadradosR n == n * (n+1) * (2*n+1) 'div' 6
----La comprobaci�n es
----ghci> quickCheck prop_SumaCuadrados
----OK, passed 100 tests.
----Ejercicio 4.1.3. Definir, por comprensi�n, la funci�n
----sumaCuadradosC :: Integer -> Integer
----tal que (sumaCuadradosC n) es la suma de los cuadrados de los n�meros de 1 a n. Por ejemplo,
----sumaCuadradosC 4 == 30
----Soluci�n:
----sumaCuadradosC :: Integer -> Integer
----sumaCuadradosC n = sum [x^2 | x <- [1..n]]
----Ejercicio 4.1.4. Comprobar con QuickCheck que las funciones sumaCuadradosR y sumaCuadradosC
----son equivalentes sobre los n�meros naturales.
----Soluci�n: La propiedad es
----prop_sumaCuadrados n =
----n >= 0 ==> sumaCuadradosR n == sumaCuadradosC n
----La comprobaci�n es
----ghci> quickCheck prop_sumaCuadrados
---- +++ OK, passed 100 tests.
----70 Cap�tulo 4. Definiciones por recursi�n y por comprensi�n
----4.2. N�mero de bloques de escaleras triangulares
----Ejercicio 4.2.1. Se quiere formar una escalera con bloques cuadrados, de forma que tenga un n�mero
----determinado de escalones. Por ejemplo, una escalera con tres escalones tendr�a la siguiente
----forma:
----XX
----XXXX
----XXXXXX
----Definir, por recursi�n, la funci�n
----numeroBloquesR :: Integer -> Integer
----tal que (numeroBloquesR n) es el n�mero de bloques necesarios para construir una escalera
----con n escalones. Por ejemplo,
----numeroBloquesR 1 == 2
----numeroBloquesR 3 == 12
----numeroBloquesR 10 == 110
----Soluci�n:
----numeroBloquesR :: Integer -> Integer
----numeroBloquesR 0 = 0
----numeroBloquesR n = 2*n + numeroBloquesR (n-1)
----Ejercicio 4.2.2. Definir, por comprensi�n, la funci�n
----numeroBloquesC :: Integer -> Integer
----tal que (numeroBloquesC n) es el n�mero de bloques necesarios para construir una escalera
----con n escalones. Por ejemplo,
----numeroBloquesC 1 == 2
----numeroBloquesC 3 == 12
----numeroBloquesC 10 == 110
----Soluci�n:
----numeroBloquesC :: Integer -> Integer
----numeroBloquesC n = sum [2*x | x <- [1..n]]
----Ejercicio 4.2.3. Comprobar con QuickCheck que (numeroBloquesC n) es igual a n + n2.
----Soluci�n: La propiedad es
----4.3. Suma de los cuadrados de los impares entre los primeros n�meros 71
----prop_numeroBloques n =
----n > 0 ==> numeroBloquesC n == n+n^2
----La comprobaci�n es
----ghci> quickCheck prop_numeroBloques
---- +++ OK, passed 100 tests.
----4.3. Suma de los cuadrados de los impares entre los primeros
----n�meros
----Ejercicio 4.3.1. Definir, por recursi�n, la funci�n
----sumaCuadradosImparesR :: Integer -> Integer
----tal que (sumaCuadradosImparesR n) es la suma de los cuadrados de los n�meros impares desde
----1 hasta n. Por ejemplo,
----sumaCuadradosImparesR 1 == 1
----sumaCuadradosImparesR 7 == 84
----sumaCuadradosImparesR 4 == 10
----Soluci�n:
----sumaCuadradosImparesR :: Integer -> Integer
----sumaCuadradosImparesR 1 = 1
----sumaCuadradosImparesR n
--- |odd n = n^2 + sumaCuadradosImparesR (n-1)
--- |otherwise = sumaCuadradosImparesR (n-1)
----Ejercicio 4.3.2. Definir, por comprensi�n, la funci�n
----sumaCuadradosImparesC :: Integer -> Integer
----tal que (sumaCuadradosImparesC n) es la suma de los cuadrados de los n�meros impares desde
----1 hasta n. Por ejemplo,
----sumaCuadradosImparesC 1 == 1
----sumaCuadradosImparesC 7 == 84
----sumaCuadradosImparesC 4 == 10
----Soluci�n:
----72 Cap�tulo 4. Definiciones por recursi�n y por comprensi�n
----sumaCuadradosImparesC :: Integer -> Integer
----sumaCuadradosImparesC n = sum [x^2 | x <- [1..n], odd x]
----Otra definici�n m�s simple es
----sumaCuadradosImparesC' :: Integer -> Integer
----sumaCuadradosImparesC' n = sum [x^2 | x <- [1,3..n]]
----4.4. Operaciones con los d�gitos de los n�meros
----4.4.1. Lista de los d�gitos de un n�mero
----Ejercicio 4.4.1. Definir, por recursi�n, la funci�n
----digitosR :: Integer -> [Int]
----tal que (digitosR n) es la lista de los d�gitos del n�mero n. Por ejemplo,
----digitosR 320274 == [3,2,0,2,7,4]
----Soluci�n:
----digitosR :: Integer -> [Integer]
----digitosR n = reverse (digitosR' n)
----digitosR' n
--- |n < 10 = [n]
--- |otherwise = (n 'rem' 10) : digitosR' (n 'div' 10)
----Ejercicio 4.4.2. Definir, por comprensi�n, la funci�n
----digitosC :: Integer -> [Int]
----tal que (digitosC n) es la lista de los d�gitos del n�mero n. Por ejemplo,
----digitosC 320274 == [3,2,0,2,7,4]
----Indicaci�n: Usar las funciones show y read.
----Soluci�n:
----digitosC :: Integer -> [Integer]
----digitosC n = [read [x] | x <- show n]
----4.4. Operaciones con los d�gitos de los n�meros 73
----Ejercicio 4.4.3. Comprobar con QuickCheck que las funciones digitosR y digitos son equivalentes.
----Soluci�n: La propiedad es
----prop_d�gitos n =
----n >= 0 ==>
----digitosR n == digitosC n
----La comprobaci�n es
----ghci> quickCheck prop_d�gitos
---- +++ OK, passed 100 tests.
----4.4.2. Suma de los d�gitos de un n�mero
----Ejercicio 4.4.4. Definir, por recursi�n, la funci�n
----sumaDigitosR :: Integer -> Integer
----tal que (sumaDigitosR n) es la suma de los d�gitos de n. Por ejemplo,
----sumaDigitosR 3 == 3
----sumaDigitosR 2454 == 15
----sumaDigitosR 20045 == 11
----Soluci�n:
----sumaDigitosR :: Integer -> Integer
----sumaDigitosR n
--- |n < 10 = n
--- |otherwise = n 'rem' 10 + sumaDigitosR (n 'div' 10)
----Ejercicio 4.4.5. Definir, sin usar recursi�n, la funci�n
----sumaDigitosNR :: Integer -> Integer
----tal que (sumaDigitosNR n) es la suma de los d�gitos de n. Por ejemplo,
----sumaDigitosNR 3 == 3
----sumaDigitosNR 2454 == 15
----sumaDigitosNR 20045 == 11
----Soluci�n:
----74 Cap�tulo 4. Definiciones por recursi�n y por comprensi�n
----sumaDigitosNR :: Integer -> Integer
----sumaDigitosNR n = sum (digitosR n)
----Ejercicio 4.4.6. Comprobar con QuickCheck que las funciones sumaDigitosR y sumaDigitosNR
----son equivalentes.
----Soluci�n: La propiedad es
----prop_sumaD�gitos n =
----n >= 0 ==>
----sumaDigitosR n == sumaDigitosNR n
----La comprobaci�n es
----ghci> quickCheck prop_sumaD�gitos
---- +++ OK, passed 100 tests.
----4.4.3. Decidir si es un d�gito del n�mero
----Ejercicio 4.4.7. Definir la funci�n
----esDigito :: Integer -> Integer -> Bool
----tal que (esDigito x n) se verifica si x es una d�gito de n. Por ejemplo,
----esDigito 4 1041 == True
----esDigito 3 1041 == False
----Soluci�n:
----esDigito :: Integer -> Integer -> Bool
----esDigito x n = elem x (digitosR n)
----4.4.4. N�mero de d�gitos de un n�mero
----Ejercicio 4.4.8. Definir la funci�n
----numeroDeDigitos :: Integer -> Integer
----tal que (numeroDeDigitos x) es el n�mero de d�gitos de x. Por ejemplo,
----numeroDeDigitos 34047 == 5
----Soluci�n:
----numeroDeDigitos :: Integer -> Int
----numeroDeDigitos x = length (digitosR x)
----4.4. Operaciones con los d�gitos de los n�meros 75
----4.4.5. N�mero correspondiente a una lista de d�gitos
----Ejercicio 4.4.9. Definir, por recursi�n, la funci�n
----listaNumeroR :: [Integer] -> Integer
----tal que (listaNumeroR xs) es el n�mero formado por los d�gitos de la lista xs. Por ejemplo,
----listaNumeroR [5] == 5
----listaNumeroR [1,3,4,7] == 1347
----listaNumeroR [0,0,1] == 1
----Soluci�n:
----listaNumeroR :: [Integer] -> Integer
----listaNumeroR xs = listaNumeroR' (reverse xs)
----listaNumeroR' :: [Integer] -> Integer
----listaNumeroR' [x] = x
----listaNumeroR' (x:xs) = x + 10 * (listaNumeroR' xs)
----Ejercicio 4.4.10. Definir, por comprensi�n, la funci�n
----listaNumeroC :: [Integer] -> Integer
----tal que (listaNumeroC xs) es el n�mero formado por los d�gitos de la lista xs. Por ejemplo,
----listaNumeroC [5] == 5
----listaNumeroC [1,3,4,7] == 1347
----listaNumeroC [0,0,1] == 1
----Soluci�n:
----listaNumeroC :: [Integer] -> Integer
----listaNumeroC xs = sum [y*10^n | (y,n) <- zip (reverse xs) [0..]]
----4.4.6. Concatenaci�n de dos n�meros
----Ejercicio 4.4.11. Definir, por recursi�n, la funci�n
----pegaNumerosR :: Integer -> Integer -> Integer
----tal que (pegaNumerosR x y) es el n�mero resultante de �pegar� los n�meros x e y. Por ejemplo,
----pegaNumerosR 12 987 == 12987
----pegaNumerosR 1204 7 == 12047
----pegaNumerosR 100 100 == 100100
----76 Cap�tulo 4. Definiciones por recursi�n y por comprensi�n
----Soluci�n:
----pegaNumerosR :: Integer -> Integer -> Integer
----pegaNumerosR x y
--- |y < 10 = 10*x+y
--- |otherwise = 10 * pegaNumerosR x (y 'div'10) + (y 'rem' 10)
----Ejercicio 4.4.12. Definir, sin usar recursi�n, la funci�n
----pegaNumerosNR :: Integer -> Integer -> Integer
----tal que (pegaNumerosNR x y) es el n�mero resultante de �pegar� los n�meros x e y. Por ejemplo,
----pegaNumerosNR 12 987 == 12987
----pegaNumerosNR 1204 7 == 12047
----pegaNumerosNR 100 100 == 100100
----Soluci�n:
----pegaNumerosNR :: Integer -> Integer -> Integer
----pegaNumerosNR x y = listaNumeroC (digitosR x ++ digitosR y)
----Ejercicio 4.4.13. Comprobar con QuickCheck que las funciones pegaNumerosR y pegaNumerosNR
----son equivalentes.
----Soluci�n: La propiedad es
----prop_pegaNumeros x y =
----x >= 0 && y >= 0 ==>
----pegaNumerosR x y == pegaNumerosNR x y
----La comprobci�n es
----ghci> quickCheck prop_pegaNumeros
---- +++ OK, passed 100 tests.
----4.4.7. Primer d�gito de un n�mero
----Ejercicio 4.4.14. Definir, por recursi�n, la funci�n
----primerDigitoR :: Integer -> Integer
----tal que (primerDigitoR n) es el primer d�gito de n. Por ejemplo,
----4.4. Operaciones con los d�gitos de los n�meros 77
----primerDigitoR 425 == 4
----Soluci�n:
----primerDigitoR :: Integer -> Integer
----primerDigitoR n
--- |n < 10 = n
--- |otherwise = primerDigitoR (n 'div' 10)
----Ejercicio 4.4.15. Definir, sin usar recursi�n, la funci�n
----primerDigitoNR :: Integer -> Integer
----tal que (primerDigitoNR n) es el primer d�gito de n. Por ejemplo,
----primerDigitoNR 425 == 4
----Soluci�n:
----primerDigitoNR :: Integer -> Integer
----primerDigitoNR n = head (digitosR n)
----Ejercicio 4.4.16. Comprobar con QuickCheck que las funciones primerDigitoR y primerDigitoNR
----son equivalentes.
----Soluci�n: La propiedad es
----prop_primerDigito x =
----x >= 0 ==>
----primerDigitoR x == primerDigitoNR x
----La comprobaci�n es
----ghci> quickCheck prop_primerDigito
---- +++ OK, passed 100 tests.
----4.4.8. �ltimo d�gito de un n�mero
----Ejercicio 4.4.17. Definir la funci�n
----ultimoDigito :: Integer -> Integer
----tal que (ultimoDigito n) es el �ltimo d�gito de n. Por ejemplo,
----ultimoDigito 425 == 5
----Soluci�n:
----ultimoDigito :: Integer -> Integer
----ultimoDigito n = n 'rem' 10
----78 Cap�tulo 4. Definiciones por recursi�n y por comprensi�n
----4.4.9. N�mero con los d�gitos invertidos
----Ejercicio 4.4.18. Definir la funci�n
----inverso :: Integer -> Integer
----tal que (inverso n) es el n�mero obtenido escribiendo los d�gitos de n en orden inverso. Por
----ejemplo,
----inverso 42578 == 87524
----inverso 203 == 302
----Soluci�n:
----inverso :: Integer -> Integer
----inverso n = listaNumeroC (reverse (digitosR n))
----Ejercicio 4.4.19. Definir, usando show y read, la funci�n
----inverso' :: Integer -> Integer
----tal que (inverso' n) es el n�mero obtenido escribiendo los d�gitos de n en orden inverso�. Por
----ejemplo,
----inverso' 42578 == 87524
----inverso' 203 == 302
----Soluci�n:
----inverso' :: Integer -> Integer
----inverso' n = read (reverse (show n))
----Ejercicio 4.4.20. Comprobar con QuickCheck que las funciones inverso e inverso' son equivalentes.
----Soluci�n: La propiedad es
----prop_inverso n =
----n >= 0 ==>
----inverso n == inverso' n
----La comprobaci�n es
----ghci> quickCheck prop_inverso
---- +++ OK, passed 100 tests.
----4.4. Operaciones con los d�gitos de los n�meros 79
----4.4.10. Decidir si un n�mero es capic�a
----Ejercicio 4.4.21. Definir la funci�n
----capicua :: Integer -> Bool
----tal que (capicua n) se verifica si los d�gitos de n son los mismos de izquierda a derecha que de
----derecha a izquierda. Por ejemplo,
----capicua 1234 = False
----capicua 1221 = True
----capicua 4 = True
----Soluci�n:
----capicua :: Integer -> Bool
----capicua n = n == inverso n
----4.4.11. Suma de los d�gitos de 21000
----En el problema 16 del proyecto Euler1 se pide calcular la suma de las d�gitos de 21000.
----Lo resolveremos mediante los distintos apartados de este ejercicio.
----Ejercicio 4.4.22. Definir la funci�n
----euler16 :: Integer -> Integer
----tal que (euler16 n) es la suma de los d�gitos de 2n. Por ejemplo,
----euler16 4 == 7
----Soluci�n:
----euler16 :: Integer -> Integer
----euler16 n = sumaDigitosNR (2^n)
----Ejercicio 4.4.23. Calcular la suma de los d�gitos de 21000.
----Soluci�n: El c�lculo es
----ghci> euler16 1000
----1366
----1http://projecteuler.net/problem=16
----80 Cap�tulo 4. Definiciones por recursi�n y por comprensi�n
----4.4.12. Primitivo de un n�mero
----Ejercicio 4.4.24. En el enunciado de uno de los problemas de las Olimpiadas matem�ticas de
----Brasil se define el primitivo de un n�mero como sigue:
----Dado un n�mero natural n, multiplicamos todos sus d�gitos, repetimos este procedimiento
----hasta que quede un solo d�gito al cual llamamos primitivo de n. Por ejemplo
----para 327 : 3  2  7 = 42 y 4  2 = 8. Por lo tanto, el primitivo de 327 es 8.
----Definir la funci�n
----primitivo :: Integer -> Integer
----tal que (primitivo n) es el primitivo de n. Por ejemplo.
----primitivo 327 == 8
----Soluci�n:
----primitivo :: Integer -> Integer
----primitivo n | n < 10 = n
--- |otherwise = primitivo (producto n)
----donde (producto n) es el producto de los d�gitos de n. Por ejemplo,
----producto 327 == 42
----producto :: Integer -> Integer
----producto = product . digitosC
----4.4.13. N�meros con igual media de sus d�gitos
----Ejercicio 4.4.25. Dos n�meros son equivalentes si la media de sus d�gitos son iguales. Por
----ejemplo, 3205 y 41 son equivalentes ya que
----3 + 2 + 0 + 5
----4
---- =
----4 + 1
----2
----Definir la funci�n
----equivalentes :: Int -> Int -> Bool
----tal que (equivalentes x y) se verifica si los n�meros x e y son equivalentes. Por ejemplo,
----equivalentes 3205 41 == True
----equivalentes 3205 25 == False
----4.4. Operaciones con los d�gitos de los n�meros 81
----Soluci�n:
----equivalentes :: Integer -> Integer -> Bool
----equivalentes x y = media (digitosC x) == media (digitosC y)
----donde (media xs) es la media de la lista xs. Por ejemplo,
----media [3,2,0,5] == 2.5
----media :: [Integer] -> Float
----media xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))
----4.4.14. N�meros con d�gitos duplicados en su cuadrado
----Ejercicio 4.4.26. Un n�mero x es especial si el n�mero de ocurrencia de cada d�gito d de x en
----x2 es el doble del n�mero de ocurrencia de d en x. Por ejemplo, 72576 es especial porque tiene un
----2, un 5, un 6 y dos 7 y su cuadrado es 5267275776 que tiene exactamente dos 2, dos 5, dos 6 y
----cuatro 7.
----Definir la funci�n
----especial :: Integer -> Bool
----tal que (especial x) se verifica si x es un n�mero especial. Por ejemplo,
----especial 72576 == True
----especial 12 == False
----Calcular el menor n�mero especial mayor que 72576.
----Soluci�n:
----especial :: Integer -> Bool
----especial x =
----sort (ys ++ ys) == sort (show (x^2))
----where ys = show x
----El c�lculo es
----ghci> head [x | x <- [72577..], especial x]
----406512
----82 Cap�tulo 4. Definiciones por recursi�n y por comprensi�n
----4.5. Cuadrados de los elementos de una lista
----Ejercicio 4.5.1. Definir, por comprensi�n, la funci�n
----cuadradosC :: [Integer] -> [Integer]
----tal que (cuadradosC xs) es la lista de los cuadrados de xs. Por ejemplo,
----cuadradosC [1,2,3] == [1,4,9]
----Soluci�n:
----cuadradosC :: [Integer] -> [Integer]
----cuadradosC xs = [x*x | x <- xs]
----Ejercicio 4.5.2. Definir, por recursi�n, la funci�n
----cuadradosR :: [Integer] -> [Integer]
----tal que (cuadradosR xs) es la lista de los cuadrados de xs. Por ejemplo,
----cuadradosR [1,2,3] == [1,4,9]
----Soluci�n:
----cuadradosR :: [Integer] -> [Integer]
----cuadradosR [] = []
----cuadradosR (x:xs) = x*x : cuadradosR xs
----Ejercicio 4.5.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Soluci�n: La propiedad es
----prop_cuadrados :: [Integer] -> Bool
----prop_cuadrados xs =
----cuadradosC xs == cuadradosR xs
----La comprobaci�n es
----ghci> quickCheck prop_cuadrados
---- +++ OK, passed 100 tests.
----4.6. N�meros impares de una lista 83
----4.6. N�meros impares de una lista
----Ejercicio 4.6.1. Definir, por comprensi�n, la funci�n
----imparesC :: [Integer] -> [Integer]
----tal que (imparesC xs) es la lista de los n�meros impares de xs. Por ejemplo,
----imparesC [1,2,4,3,6] == [1,3]
----Soluci�n:
----imparesC :: [Integer] -> [Integer]
----imparesC xs = [x | x <- xs, odd x]
----Ejercicio 4.6.2. Definir, por recursi�n, la funci�n
----imparesR :: [Integer] -> [Integer]
----tal que (imparesR xs) es la lista de los n�meros impares de xs. Por ejemplo,
----imparesR [1,2,4,3,6] == [1,3]
----Soluci�n:
----imparesR :: [Integer] -> [Integer]
----imparesR [] = []
----imparesR (x:xs) | odd x = x : imparesR xs
--- |otherwise = imparesR xs
----Ejercicio 4.6.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Soluci�n: La propiedad es
----prop_impares :: [Integer] -> Bool
----prop_impares xs =
----imparesC xs == imparesR xs
----La comprobaci�n es
----ghci> quickCheck prop_impares
---- +++ OK, passed 100 test
----
--
--
-------CADENAS
----
----5.1. Suma de los d�gitos de una cadena
----Ejercicio 5.1.1. Definir, por comprensi�n, la funci�n
----sumaDigitosC :: String -> Int
----tal que (sumaDigitosC xs) es la suma de los d�gitos de la cadena xs. Por ejemplo,
----103
----104 Cap�tulo 5. Funciones sobre cadenas
----sumaDigitosC "SE 2431 X" == 10
----Nota: Usar las funciones isDigit y digitToInt.
----Soluci�n:
----sumaDigitosC :: String -> Int
----sumaDigitosC xs = sum [digitToInt x | x <- xs, isDigit x]
----Ejercicio 5.1.2. Definir, por recursi�n, la funci�n
----sumaDigitosR :: String -> Int
----tal que (sumaDigitosR xs) es la suma de los d�gitos de la cadena xs. Por ejemplo,
----sumaDigitosR "SE 2431 X" == 10
----Nota: Usar las funciones isDigit y digitToInt.
----Soluci�n:
----sumaDigitosR :: String -> Int
----sumaDigitosR [] = 0
----sumaDigitosR (x:xs)
--- |isDigit x = digitToInt x + sumaDigitosR xs
--- |otherwise = sumaDigitosR xs
----Ejercicio 5.1.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Soluci�n: La propiedad es
----prop_sumaDigitos :: String -> Bool
----prop_sumaDigitos xs =
----sumaDigitosC xs == sumaDigitosR xs
----La comprobaci�n es
----ghci> quickCheck prop_sumaDigitos
---- +++ OK, passed 100 tests.
----5.2. Capitalizaci�n de una cadena 105
----5.2. Capitalizaci�n de una cadena
----Ejercicio 5.2.1. Definir, por comprensi�n, la funci�n
----mayusculaInicial :: String -> String
----tal que (mayusculaInicial xs) es la palabra xs con la letra inicial en may�scula y las restantes
----en min�sculas. Por ejemplo,
----mayusculaInicial "sEviLLa" == "Sevilla"
----Nota: Usar las funciones toLowery toUpper.
----Soluci�n:
----mayusculaInicial :: String -> String
----mayusculaInicial [] = []
----mayusculaInicial (x:xs) = toUpper x : [toLower x | x <- xs]
----Ejercicio 5.2.2. Definir, por recursi�n, la funci�n
----mayusculaInicialR :: String -> String
----tal que (mayusculaInicialR xs) es la palabra xs con la letra inicial en may�scula y las restantes
----en min�sculas. Por ejemplo,
----mayusculaInicialR "sEviLLa" == "Sevilla"
----Soluci�n:
----mayusculaInicialR :: String -> String
----mayusculaInicialR [] = []
----mayusculaInicialR (x:xs) = toUpper x : aux xs
----where aux (x:xs) = toLower x : aux xs
----aux [] = []
----Ejercicio 5.2.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Soluci�n: La propiedad es
----prop_mayusculaInicial :: String -> Bool
----prop_mayusculaInicial xs =
----mayusculaInicial xs == mayusculaInicialR xs
----La comprobaci�n es
----ghci> quickCheck prop_mayusculaInicial
---- +++ OK, passed 100 tests.
----106 Cap�tulo 5. Funciones sobre cadenas
----5.3. T�tulo con las reglas de may�sculas iniciales
----Ejercicio 5.3.1. Se consideran las siguientes reglas de may�sculas iniciales para los t�tulos:
----la primera palabra comienza en may�scula y
----todas las palabras que tienen 4 letras como m�nimo empiezan con may�sculas.
----Definir, por comprensi�n, la funci�n
----titulo :: [String] -> [String]
----tal que (titulo ps) es la lista de las palabras de ps con las reglas de may�sculas iniciales de
----los t�tulos. Por ejemplo,
----ghci> titulo ["eL","arTE","DE","La","proGraMacion"]
----["El","Arte","de","la","Programacion"]
----Soluci�n:
----titulo :: [String] -> [String]
----titulo [] = []
----titulo (p:ps) = mayusculaInicial p : [transforma p | p <- ps]
----donde (transforma p) es la palabra p con may�scula inicial si su longitud es mayor o
----igual que 4 y es p en min�scula en caso contrario.
----transforma :: String -> String
----transforma p | length p >= 4 = mayusculaInicial p
--- |otherwise = minuscula p
----y (minuscula xs) es la palabra xs en min�scula.
----minuscula :: String -> String
----minuscula xs = [toLower x | x <- xs]
----Ejercicio 5.3.2. Definir, por recursi�n, la funci�n
----tituloR :: [String] -> [String]
----tal que (tituloR ps) es la lista de las palabras de ps con las reglas de may�sculas iniciales de
----los t�tulos. Por ejemplo,
----ghci> tituloR ["eL","arTE","DE","La","proGraMacion"]
----["El","Arte","de","la","Programacion"]
----5.4. B�squeda en crucigramas 107
----Soluci�n:
----tituloR :: [String] -> [String]
----tituloR [] = []
----tituloR (p:ps) = mayusculaInicial p : tituloRAux ps
----where tituloRAux [] = []
----tituloRAux (p:ps) = transforma p : tituloRAux ps
----Ejercicio 5.3.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Soluci�n: La propiedad es
----prop_titulo :: [String] -> Bool
----prop_titulo xs = titulo xs == tituloR xs
----La comprobaci�n es
----ghci> quickCheck prop_titulo
---- +++ OK, passed 100 tests.
----5.4. B�squeda en crucigramas
----Ejercicio 5.4.1. Definir, por comprensi�n, la funci�n
----buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
----tal que (buscaCrucigrama l pos lon ps) es la lista de las palabras de la lista de palabras ps
----que tienen longitud lon y poseen la letra l en la posici�n pos (comenzando en 0). Por ejemplo,
----ghci> buscaCrucigrama 'c' 1 7 ["ocaso", "acabado", "ocupado"]
----["acabado","ocupado"]
----Soluci�n:
----buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
----buscaCrucigrama l pos lon ps =
----[p | p <- ps,
----length p == lon,
----0 <= pos, pos < length p,
----p !! pos == l]
----Ejercicio 5.4.2. Definir, por recursi�n, la funci�n
----buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
----108 Cap�tulo 5. Funciones sobre cadenas
----tal que (buscaCrucigramaR l pos lon ps) es la lista de las palabras de la lista de palabras
----ps que tienen longitud lon y posen la letra l en la posici�n pos (comenzando en 0). Por ejemplo,
----ghci> buscaCrucigramaR 'c' 1 7 ["ocaso", "acabado", "ocupado"]
----["acabado","ocupado"]
----Soluci�n:
----buscaCrucigramaR :: Char -> Int -> Int -> [String] -> [String]
----buscaCrucigramaR letra pos lon [] = []
----buscaCrucigramaR letra pos lon (p:ps)
--- |length p == lon && 0 <= pos && pos < length p && p !! pos == letra
---- = p : buscaCrucigramaR letra pos lon ps
--- |otherwise
---- = buscaCrucigramaR letra pos lon ps
----Ejercicio 5.4.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Soluci�n: La propiedad es
----prop_buscaCrucigrama :: Char -> Int -> Int -> [String] -> Bool
----prop_buscaCrucigrama letra pos lon ps =
----buscaCrucigrama letra pos lon ps == buscaCrucigramaR letra pos lon ps
----La comprobaci�n es
----ghci> quickCheck prop_buscaCrucigrama
---- +++ OK, passed 100 tests.
----5.5. Posiciones de un car�cter en una cadena
----Ejercicio 5.5.1. Definir, por comprensi�n, la funci�n
----posiciones :: String -> Char -> [Int]
----tal que (posiciones xs y) es la lista de la posiciones del car�cter y en la cadena xs. Por
----ejemplo,
----posiciones "Salamamca" 'a' == [1,3,5,8]
----Soluci�n:
----posiciones :: String -> Char -> [Int]
----posiciones xs y = [n | (x,n) <- zip xs [0..], x == y]
----5.6. Decidir si una cadena es subcadena de otra 109
----Ejercicio 5.5.2. Definir, por recursi�n, la funci�n
----posicionesR :: String -> Char -> [Int]
----tal que (posicionesR xs y) es la lista de la posiciones del car�cter y en la cadena xs. Por
----ejemplo,
----posicionesR "Salamamca" 'a' == [1,3,5,8]
----Soluci�n:
----posicionesR :: String -> Char -> [Int]
----posicionesR xs y = posicionesAux xs y 0
----where
----posicionesAux [] y n = []
----posicionesAux (x:xs) y n | x == y = n : posicionesAux xs y (n+1)
--- |otherwise = posicionesAux xs y (n+1)
----Ejercicio 5.5.3. Comprobar con QuickCheck que ambas definiciones son equivalentes.
----Soluci�n: La propiedad es
----prop_posiciones :: String -> Char -> Bool
----prop_posiciones xs y =
----posiciones xs y == posicionesR xs y
----La comprobaci�n es
----ghci> quickCheck prop_posiciones
---- +++ OK, passed 100 tests.
----5.6. Decidir si una cadena es subcadena de otra
----Ejercicio 5.6.1. Definir, por recursi�n, la funci�n
----contieneR :: String -> String -> Bool
----tal que (contieneR xs ys) se verifica si ys es una subcadena de xs. Por ejemplo,
----contieneR "escasamente" "casa" == True
----contieneR "escasamente" "cante" == False
----contieneR "" "" == True
----Nota: Se puede usar la predefinida (isPrefixOf ys xs) que se verifica si ys es un prefijo de
----xs.
----110 Cap�tulo 5. Funciones sobre cadenas
----Soluci�n:
----contieneR :: String -> String -> Bool
----contieneR _ [] = True
----contieneR [] ys = False
----contieneR xs ys = isPrefixOf ys xs || contieneR (tail xs) ys
----Ejercicio 5.6.2. Definir, por comprensi�n, la funci�n
----contiene :: String -> String -> Bool
----tal que (contiene xs ys) se verifica si ys es una subcadena de xs. Por ejemplo,
----contiene "escasamente" "casa" == True
----contiene "escasamente" "cante" == False
----contiene "casado y casada" "casa" == True
----contiene "" "" == True
----Nota: Se puede usar la predefinida (isPrefixOf ys xs) que se verifica si ys es un prefijo de
----xs.
----Soluci�n:
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
----Soluci�n: La propiedad es
----5.7. Codificaci�n de mensajes 111
----prop_contiene :: String -> String -> Bool
----prop_contiene xs ys =
----contieneR xs ys == contiene xs ys
----La comprobaci�n es
----ghci> quickCheck prop_contiene
---- +++ OK, passed 100 tests.
----5.7. Codificaci�n de mensajes
----Se desea definir una funci�n que codifique mensajes tales como
----eres lo que piensas
----del siguiente modo:
----(a) se separa la cadena en la lista de sus palabras:
----["eres","lo","que","piensas"]
----(b) se cuenta las letras de cada palabra:
----[4,2,3,7]
----(c) se une todas las palabras:
----"eresloquepiensas"
----(d) se reagrupa las letras de 4 en 4, dejando el �ltimo grupo con el resto:
----["eres","loqu","epie","nsas"]
----(e) se inverte cada palabra:
----["sere","uqol","eipe","sasn"]
----(f) se une todas las palabras:
----"sereuqoleipesasn"
----(g) se reagrupan tal como indica la inversa de la lista del apartado (b):
----["sereuqo","lei","pe","sasn"]
----(h) se crea una frase con las palabras anteriores separadas por un espacio en blanco
----112 Cap�tulo 5. Funciones sobre cadenas
----"sereuqo lei pe sasn"
----obteniendo as� el mensaje codificado.
----En los distintos apartados de esta secci�n se definir� el anterior proceso de codificaci�n.
----Ejercicio 5.7.1. Definir la funci�n
----divide :: (a -> Bool) -> [a] -> ([a], [a])
----tal que (divide p xs) es el par (ys,zs) donde ys es el mayor prefijo de xs cuyos elementos
----cumplen p y zs es la lista de los restantes elementos de xs. Por ejemplo,
----divide (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
----divide (< 9) [1,2,3] == ([1,2,3],[])
----divide (< 0) [1,2,3] == ([],[1,2,3])
----Soluci�n:
----divide :: (a -> Bool) -> [a] -> ([a], [a])
----divide p xs = (takeWhile p xs, dropWhile p xs)
----Es equivalente a la predefinida span
----divide' :: (a -> Bool) -> [a] -> ([a], [a])
----divide' = span
----Ejercicio 5.7.2. Definir la funci�n
----palabras :: String -> [String]
----tal que (palabras cs) es la lista de las palabras de la cadena cs. Por ejemplo,
----palabras "eres lo que piensas" == ["eres","lo","que","piensas"]
----Soluci�n:
----palabras :: String -> [String]
----palabras [] = []
----palabras cs = cs1 : palabras cs2
----where cs' = dropWhile (==' ') cs
----(cs1,cs2) = divide (/=' ') cs'
----Es equivalente a la predefinida words
----5.7. Codificaci�n de mensajes 113
----palabras' :: String -> [String]
----palabras' = words
----Ejercicio 5.7.3. Definir la funci�n
----longitudes :: [[a]] -> [Int]
----tal que (longitudes xss) es la lista de las longitudes de los elementos xss. Por ejemplo,
----longitudes ["eres","lo","que","piensas"] == [4,2,3,7]
----Soluci�n:
----longitudes :: [[a]] -> [Int]
----longitudes = map length
----Ejercicio 5.7.4. Definir la funci�n
----une :: [[a]] -> [a]
----tal que (une xss) es la lista obtenida uniendo los elementos de xss. Por ejemplo,
----une ["eres","lo","que","piensas"] == "eresloquepiensas"
----Soluci�n:
----une :: [[a]] -> [a]
----une = concat
----Ejercicio 5.7.5. Definir la funci�n
----reagrupa :: [a] -> [[a]]
----tal que (reagrupa xs) es la lista obtenida agrupando los elementos de xs de 4 en 4. Por ejemplo,
----reagrupa "eresloquepiensas" == ["eres","loqu","epie","nsas"]
----reagrupa "erestu" == ["eres","tu"]
----Soluci�n:
----reagrupa :: [a] -> [[a]]
----reagrupa [] = []
----reagrupa xs = take 4 xs : reagrupa (drop 4 xs)
----Ejercicio 5.7.6. Definir la funci�n
----114 Cap�tulo 5. Funciones sobre cadenas
----inversas :: [[a]] -> [[a]]
----tal que (inversas xss) es la lista obtenida invirtiendo los elementos de xss. Por ejemplo,
----ghci> inversas ["eres","loqu","epie","nsas"]
----["sere","uqol","eipe","sasn"]
----ghci> une (inversas ["eres","loqu","epie","nsas"])
----"sereuqoleipesasn"
----Soluci�n:
----inversas :: [[a]] -> [[a]]
----inversas = map reverse
----Ejercicio 5.7.7. Definir la funci�n
----agrupa :: [a] -> [Int] -> [[a]]
----tal que (agrupa xs ns) es la lista obtenida agrupando los elementos de xs seg�n las longitudes
----indicadas en ns. Por ejemplo,
----ghci> agrupa "sereuqoleipesasn" [7,3,2,4]
----["sereuqo","lei","pe","sasn"]
----Soluci�n:
----agrupa :: [a] -> [Int] -> [[a]]
----agrupa [] _ = []
----agrupa xs (n:ns) = (take n xs) : (agrupa (drop n xs) ns)
----Ejercicio 5.7.8. Definir la funci�n
----frase :: [String] -> String
----tal que (frase xs) es la frase obtenida las palabras de xs dejando un espacio en blanco entre
----ellas. Por ejemplo,
----frase ["sereuqo","lei","pe","sasn"] == "sereuqo lei pe sasn"
----Soluci�n:
----frase :: [String] -> String
----frase [x] = x
----frase (x:xs) = x ++ " " ++ frase xs
----frase [] = []
----5.8. N�meros de ceros finales 115
----La funci�n frase es equivalente a unwords.
----frase' :: [String] -> String
----frase' = unwords
----Ejercicio 5.7.9. Definir la funci�n
----clave :: String -> String
----que realice el proceso completo. Por ejemplo,
----clave "eres lo que piensas" == "sereuqo lei pe sasn"
----Soluci�n:
----clave :: String -> String
----clave xss = frase (agrupa (une (inversas (reagrupa (une ps))))
----(reverse (longitudes ps)))
----where ps = palabras xss
----5.8. N�meros de ceros finales
----Ejercicio 5.8.1. Definir, por recursi�n, la funci�n
----ceros :: Int -> Int
----tal que (ceros n) es el n�mero de ceros en los que termina el n�mero n. Por ejemplo,
----ceros 30500 == 2
----ceros 30501 == 0
----Soluci�n:
----ceros :: Int -> Int
----ceros n | n 'rem' 10 == 0 = 1 + ceros (n 'div'10)
--- |otherwise = 0
----Ejercicio 5.8.2. Definir, sin recursi�n, la funci�n
----ceros' :: Int -> Int
----tal que (ceros' n) es el n�mero de ceros en los que termina el n�mero n. Por ejemplo,
----ceros' 30500 == 2
----ceros' 30501 == 0
----Soluci�n:
----ceros' :: Int -> Int
----ceros' n = length (takeWhile (=='0') (reverse (show n)))
----116 Cap�tulo 5. Funciones sobre
----
---- FUNCIONES DE ORDEN SUPERIOR
--6.1. Segmento inicial verificando una propiedad
--Ejercicio 6.1.1. Redefinir por recursi�n la funci�n
--takeWhile :: (a -> Bool) -> [a] -> [a]
--tal que (takeWhile p xs) es la lista de los elemento de xs hasta el primero que no cumple la
--propiedad p. Por ejemplo,
--takeWhile (<7) [2,3,9,4,5] == [2,3]
--Soluci�n:
--takeWhile' :: (a -> Bool) -> [a] -> [a]
--takeWhile' _ [] = []
--takeWhile' p (x:xs)
-- | p x = x : takeWhile' p xs
-- | otherwise = []
--6.2. Complementario del segmento inicial verificando una
--propiedad
--Ejercicio 6.2.1. Redefinir por recursi�n la funci�n
--dropWhile :: (a -> Bool) -> [a] -> [a]
--tal que (dropWhile p xs) es la lista obtenida eliminando los elemento de xs hasta el primero
--que cumple la propiedad p. Por ejemplo,
--dropWhile (<7) [2,3,9,4,5] => [9,4,5]
--Soluci�n:
--dropWhile' :: (a -> Bool) -> [a] -> [a]
--dropWhile' _ [] = []
--dropWhile' p (x:xs)
-- | p x = dropWhile' p xs
-- | otherwise = x:xs
--6.3. Concatenaci�n de una lista de listas 119
--6.3. Concatenaci�n de una lista de listas
--Ejercicio 6.3.1. Redefinir, por recursi�n, la funci�n concat. Por ejemplo,
--concatR [[1,3],[2,4,6],[1,9]] == [1,3,2,4,6,1,9]
--Soluci�n:
--concatR :: [[a]] -> [a]
--concatR [] = []
--concatR (xs:xss) = xs ++ concatR xss
--Ejercicio 6.3.2. Redefinir, usando foldr, la funci�n concat. Por ejemplo,
--concatP [[1,3],[2,4,6],[1,9]] == [1,3,2,4,6,1,9]
--Soluci�n:
--concatP :: [[a]] -> [a]
--concatP = foldr (++) []
--6.4. Divisi�n de una lista num�rica seg�n su media
--Ejercicio 6.4.1. La funci�n
--divideMedia :: [Double] -> ([Double],[Double])
--dada una lista num�rica, xs, calcula el par (ys,zs), donde ys contiene los elementos de xs
--estrictamente menores que la media, mientras que zs contiene los elementos de xs estrictamente
--mayores que la media. Por ejemplo,
--divideMedia [6,7,2,8,6,3,4] == ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
--divideMedia [1,2,3] == ([1.0],[3.0])
--Definir la funci�n divideMedia por filtrado, comprensi�n y recursi�n.
--Soluci�n: La definici�n por filtrado es
--divideMediaF :: [Double] -> ([Double],[Double])
--divideMediaF xs = (filter (<m) xs, filter (>m) xs)
--where m = media xs
--donde (media xs) es la media de xs. Por ejemplo,
--120 Cap�tulo 6. Funciones de orden superior
--media [1,2,3] == 2.0
--media [1,-2,3.5,4] == 1.625
--media :: [Double] -> Double
--media xs = (sum xs) / fromIntegral (length xs)
--En la definici�n de media se usa la funci�n fromIntegral tal que (fromIntegral x) es
--el n�mero real correspondiente al n�mero entero x.
--La definici�n por comprensi�n es
--divideMediaC :: [Double] -> ([Double],[Double])
--divideMediaC xs = ([x | x <- xs, x < m], [x | x <- xs, x > m])
--where m = media xs
--La definici�n por recursi�n es
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
--Soluci�n: La propiedad es
--prop_divideMedia :: [Double] -> Bool
--prop_divideMedia xs =
--divideMediaC xs == d &&
--divideMediaR xs == d
--where d = divideMediaF xs
--La comprobaci�n es
--ghci> quickCheck prop_divideMedia
-- +++ OK, passed 100 tests.
--6.4. Divisi�n de una lista num�rica seg�n su media 121
--Ejercicio 6.4.3. Comprobar con QuickCheck que si (ys,zs) es el par obtenido aplic�ndole la
--funci�n divideMediaF a xs, entonces la suma de las longitudes de ys y zs es menor o igual que
--la longitud de xs.
--Soluci�n: La propiedad es
--prop_longitudDivideMedia :: [Double] -> Bool
--prop_longitudDivideMedia xs =
--length ys + length zs <= length xs
--where (ys,zs) = divideMediaF xs
--La comprobaci�n es
--ghci> quickCheck prop_longitudDivideMedia
-- +++ OK, passed 100 tests.
--Ejercicio 6.4.4. Comprobar con QuickCheck que si (ys,zs) es el par obtenido aplic�ndole la
--funci�n divideMediaF a xs, entonces todos los elementos de ys son menores que todos los
--elementos de zs.
--Soluci�n: La propiedad es
--prop_divideMediaMenores :: [Double] -> Bool
--prop_divideMediaMenores xs =
--and [y < z | y <- ys, z <- zs]
--where (ys,zs) = divideMediaF xs
--La comprobaci�n es
--ghci> quickCheck prop_divideMediaMenores
-- +++ OK, passed 100 tests.
--Ejercicio 6.4.5. Comprobar con QuickCheck que si (ys,zs) es el par obtenido aplic�ndole la
--funci�n divideMediaF a xs, entonces la media de xs no pertenece a ys ni a zs.
--Nota: Usar la funci�n notElem tal que (notElem x ys) se verifica si y no pertenece a ys.
--Soluci�n: La propiedad es
--prop_divideMediaSinMedia :: [Double] -> Bool
--prop_divideMediaSinMedia xs =
--notElem m (ys ++ zs)
--where m = media xs
--(ys,zs) = divideMediaF xs
--La comprobaci�n es
--ghci> quickCheck prop_divideMediaSinMedia
-- +++ OK, passed 100 tests.
--122 Cap�tulo 6. Funciones de orden superior
--6.5. Segmentos cuyos elementos verifican una propiedad
--Ejercicio 6.5.1. Definir la funci�n
--segmentos :: (a -> Bool) -> [a] -> [a]
--tal que (segmentos p xs) es la lista de los segmentos de xs cuyos elementos verifican la propiedad
--p. Por ejemplo,
--segmentos even [1,2,0,4,5,6,48,7,2] == [[],[2,0,4],[6,48],[2]]
--Soluci�n:
--segmentos :: (a -> Bool) -> [a] -> [[a]]
--segmentos _ [] = []
--segmentos p xs =
--takeWhile p xs : (segmentos p (dropWhile (not.p) (dropWhile p xs)))
--6.6. Listas con elementos consecutivos relacionados
--Ejercicio 6.6.1. Definir la funci�n
--relacionados :: (a -> a -> Bool) -> [a] -> Bool
--tal que (relacionados r xs) se verifica si para todo par (x,y) de elementos consecutivos de
--xs se cumple la relaci�n r. Por ejemplo,
--relacionados (<) [2,3,7,9] == True
--relacionados (<) [2,3,1,9] == False
--relacionados equivalentes [3205,50,5014] == True
--Soluci�n:
--relacionados :: (a -> a -> Bool) -> [a] -> Bool
--relacionados r (x:y:zs) = (r x y) && relacionados r (y:zs)
--relacionados _ _ = True
--Una definici�n alternativa es
--relacionados' :: (a -> a -> Bool) -> [a] -> Bool
--relacionados' r xs = and [r x y | (x,y) <- zip xs (tail xs)]
--6.7. Agrupamiento de elementos de una lista de listas 123
--6.7. Agrupamiento de elementos de una lista de listas
--Ejercicio 6.7.1. Definir la funci�n
--agrupa :: Eq a => [[a]] -> [[a]]
--tal que (agrupa xss) es la lista de las listas obtenidas agrupando los primeros elementos, los
--segundos, . . . de forma que las longitudes de las lista del resultado sean iguales a la m�s corta de
--xss. Por ejemplo,
--agrupa [[1..6],[7..9],[10..20]] == [[1,7,10],[2,8,11],[3,9,12]]
--agrupa [] == []
--Soluci�n:
--agrupa :: Eq a => [[a]] -> [[a]]
--agrupa [] = []
--agrupa xss
-- | [] 'elem' xss = []
-- | otherwise = primeros xss : agrupa (restos xss)
--where primeros = map head
--restos = map tail
--6.8. N�meros con d�gitos pares
--Ejercicio 6.8.1. Definir, por recursi�n, la funci�n
--superpar :: Int -> Bool
--tal que (superpar n) se verifica si n es un n�mero par tal que todos sus d�gitos son pares. Por
--ejemplo,
--superpar 426 == True
--superpar 456 == False
--Soluci�n:
--superpar :: Int -> Bool
--superpar n | n < 10 = even n
-- | otherwise = even n && superpar (n 'div' 10)
--Ejercicio 6.8.2. Definir, por comprensi�n, la funci�n
--superpar2 :: Int -> Bool
--124 Cap�tulo 6. Funciones de orden superior
--tal que (superpar2 n) se verifica si n es un n�mero par tal que todos sus d�gitos son pares. Por
--ejemplo,
--superpar2 426 == True
--superpar2 456 == False
--Soluci�n:
--superpar2 :: Int -> Bool
--superpar2 n = and [even d | d <- digitos n]
--Donde (digitos n) es la lista de los d�gitos de n.
--digitos :: Int -> [Int]
--digitos n = [read [d] | d <- show n]
--Ejercicio 6.8.3. Definir, por recursi�n sobre los d�gitos, la funci�n
--superpar3 :: Int -> Bool
--tal que (superpar3 n) se verifica si n es un n�mero par tal que todos sus d�gitos son pares. Por
--ejemplo,
--superpar3 426 == True
--superpar3 456 == False
--Soluci�n:
--superpar3 :: Int -> Bool
--superpar3 n = sonPares (digitos n)
--where sonPares [] = True
--sonPares (d:ds) = even d && sonPares ds
--Ejercicio 6.8.4. Definir, usando all, la funci�n
--superpar4 :: Int -> Bool
--tal que (superpar4 n) se verifica si n es un n�mero par tal que todos sus d�gitos son pares. Por
--ejemplo,
--superpar4 426 == True
--superpar4 456 == False
--Soluci�n:
--6.9. Lista de los valores de los elementos que cumplen una propiedad 125
--superpar4 :: Int -> Bool
--superpar4 n = all even (digitos n)
--Ejercicio 6.8.5. Definir, usando filter, la funci�n
--superpar5 :: Int -> Bool
--tal que (superpar5 n) se verifica si n es un n�mero par tal que todos sus d�gitos son pares. Por
--ejemplo,
--superpar5 426 == True
--superpar5 456 == False
--Soluci�n:
--superpar5 :: Int -> Bool
--superpar5 n = filter even (digitos n) == digitos n
--6.9. Lista de los valores de los elementos que cumplen
--una propiedad
--Ejercicio 6.9.1. Se considera la funci�n
--filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--tal que (filtraAplica f p xs) es la lista obtenida aplic�ndole a los elementos de xs que
--cumplen el predicado p la funci�n f. Por ejemplo,
--filtraAplica (4+) (<3) [1..7] => [5,6]
--Se pide, definir la funci�n
--1. por comprensi�n,
--2. usando map y filter,
--3. por recursi�n y
--4. por plegado (con foldr).
--Soluci�n: La definici�n con lista de comprensi�n es
--filtraAplica_1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_1 f p xs = [f x | x <- xs, p x]
--126 Cap�tulo 6. Funciones de orden superior
--La definici�n con map y filter es
--filtraAplica_2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_2 f p xs = map f (filter p xs)
--La definici�n por recursi�n es
--filtraAplica_3 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_3 f p [] = []
--filtraAplica_3 f p (x:xs) | p x = f x : filtraAplica_3 f p xs
-- | otherwise = filtraAplica_3 f p xs
--La definici�n por plegado es
--filtraAplica_4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_4 f p = foldr g []
--where g x y | p x = f x : y
-- | otherwise = y
--La definici�n por plegado usando lambda es
--filtraAplica_4' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
--filtraAplica_4' f p =
--foldr (\x y -> if p x then (f x : y) else y) []
--6.10. M�ximo elemento de una lista
--Ejercicio 6.10.1. Definir, mediante recursi�n, la funci�n
--maximumR :: Ord a => [a] -> a
--tal que (maximumR xs) es el m�ximo de la lista xs. Por ejemplo,
--maximumR [3,7,2,5] == 7
--Nota: La funci�n maximumR es equivalente a la predefinida maximum.
--Soluci�n:
--maximumR :: Ord a => [a] -> a
--maximumR [x] = x
--maximumR (x:y:ys) = max x (maximumR (y:ys))
--6.11. M�nimo elemento de una lista 127
--Ejercicio 6.10.2. La funci�n de plegado foldr1 est� definida por
--foldr1 :: (a -> a -> a) -> [a] -> a
--foldr1 _ [x] = x
--foldr1 f (x:xs) = f x (foldr1 f xs)
--Definir, mediante plegado con foldr1, la funci�n
--maximumP :: Ord a => [a] -> a
--tal que (maximumR xs) es el m�ximo de la lista xs. Por ejemplo,
--maximumP [3,7,2,5] == 7
--Nota: La funci�n maximumP es equivalente a la predefinida maximum.
--Soluci�n:
--maximumP :: Ord a => [a] -> a
--maximumP = foldr1 max
--6.11. M�nimo elemento de una lista
--Ejercicio 6.11.1. Definir, mediante plegado con foldr1, la funci�n
--minimunP :: Ord a => [a] -> a
--tal que (minimunR xs) es el m�ximo de la lista xs. Por ejemplo,
--minimunP [3,7,2,5] == 2
--Nota: La funci�n minimunP es equivalente a la predefinida minimun.
--Soluci�n:
--minimumP :: Ord a => [a] -> a
--minimumP = foldr1 min
--6.12. Inversa de una lista
--Ejercicio 6.12.1. Definir, mediante recursi�n, la funci�n
--inversaR :: [a] -> [a]
--tal que (inversaR xs) es la inversa de la lista xs. Por ejemplo,
--128 Cap�tulo 6. Funciones de orden superior
--inversaR [3,5,2,4,7] == [7,4,2,5,3]
--Soluci�n:
--inversaR :: [a] -> [a]
--inversaR [] = []
--inversaR (x:xs) = (inversaR xs) ++ [x]
--Ejercicio 6.12.2. Definir, mediante plegado, la funci�n
--inversaP :: [a] -> [a]
--tal que (inversaP xs) es la inversa de la lista xs. Por ejemplo,
--inversaP [3,5,2,4,7] == [7,4,2,5,3]
--Soluci�n:
--inversaP :: [a] -> [a]
--inversaP = foldr f []
--where f x y = y ++ [x]
--La definici�n anterior puede simplificarse a
--inversaP_2 :: [a] -> [a]
--inversaP_2 = foldr f []
--where f x = (++ [x])
--Ejercicio 6.12.3. Definir, por recursi�n con acumulador, la funci�n
--inversaR' :: [a] -> [a]
--tal que (inversaR' xs) es la inversa de la lista xs. Por ejemplo,
--inversaR' [3,5,2,4,7] == [7,4,2,5,3]
--Soluci�n:
--inversaR' :: [a] -> [a]
--inversaR' xs = inversaAux [] xs
--where inversaAux ys [] = ys
--inversaAux ys (x:xs) = inversaAux (x:ys) xs
--Ejercicio 6.12.4. La funci�n de plegado foldl est� definida por
--6.12. Inversa de una lista 129
--foldl :: (a -> b -> a) -> a -> [b] -> a
--foldl f ys xs = aux ys xs
--where aux ys [] = ys
--aux ys (x:xs) = aux (f ys x) xs
--Definir, mediante plegado con foldl, la funci�n
--inversaP' :: [a] -> [a]
--tal que (inversaP' xs) es la inversa de la lista xs. Por ejemplo,
--inversaP' [3,5,2,4,7] == [7,4,2,5,3]
--Soluci�n:
--inversaP' :: [a] -> [a]
--inversaP' = foldl f []
--where f ys x = x:ys
--La definici�n anterior puede simplificarse lambda:
--inversaP'_2 :: [a] -> [a]
--inversaP'_2= foldl (\ys x -> x:ys) []
--La definici�n puede simplificarse usando flip:
--inversaP'_3 :: [a] -> [a]
--inversaP'_3 = foldl (flip(:)) []
--Ejercicio 6.12.5. Comprobar con QuickCheck que las funciones reverse, inversaP e inversaP'
--son equivalentes.
--Soluci�n: La propiedad es
--prop_inversa :: Eq a => [a] -> Bool
--prop_inversa xs =
--inversaP xs == ys &&
--inversaP' xs == ys
--where ys = reverse xs
--La comprobaci�n es
--ghci> quickCheck prop_inversa
-- +++ OK, passed 100 tests.
--130 Cap�tulo 6. Funciones de orden superior
--Ejercicio 6.12.6. Comparar la eficiencia de inversaP e inversaP' calculando el tiempo y el
--espacio que usado en evaluar las siguientes expresiones:
--head (inversaP [1..100000])
--head (inversaP' [1..100000])
--Soluci�n: La sesi�n es
--ghci> :set +s
--ghci> head (inversaP [1..100000])
--100000
--(0.41 secs, 20882460 bytes)
--ghci> head (inversaP' [1..100000])
--1
--(0.00 secs, 525148 bytes)
--ghci> :unset +s
--6.13. N�mero correspondiente a la lista de sus cifras
--Ejercicio 6.13.1. Definir, por recursi�n con acumulador, la funci�n
--dec2entR :: [Int] -> Int
--tal que (dec2entR xs) es el entero correspondiente a la expresi�n decimal xs. Por ejemplo,
--dec2entR [2,3,4,5] == 2345
--Soluci�n:
--dec2entR :: [Int] -> Int
--dec2entR xs = dec2entR' 0 xs
--where dec2entR' a [] = a
--dec2entR' a (x:xs) = dec2entR' (10*a+x) xs
--Ejercicio 6.13.2. Definir, por plegado con foldl, la funci�n
--dec2entP :: [Int] -> Int
--tal que (dec2entP xs) es el entero correspondiente a la expresi�n decimal xs. Por ejemplo,
--dec2entP [2,3,4,5] == 2345
--Soluci�n:
--6.14. Suma de valores de una aplicaci�n a una lista 131
--dec2entP :: [Int] -> Int
--dec2entP = foldl f 0
--where f a x = 10*a+x
--La definici�n puede simplificarse usando lambda:
--dec2entP' :: [Int] -> Int
--dec2entP' = foldl (\a x -> 10*a+x) 0
--6.14. Suma de valores de una aplicaci�n a una lista
--Ejercicio 6.14.1. Definir, por recursi�n, la funci�n
--sumaR :: Num b => (a -> b) -> [a] -> b
--tal que (suma f xs) es la suma de los valores obtenido aplicando la funci�n f a lo elementos de
--la lista xs. Por ejemplo,
--sumaR (*2) [3,5,10] == 36
--sumaR (/10) [3,5,10] == 1.8
--Soluci�n:
--sumaR :: Num b => (a -> b) -> [a] -> b
--sumaR f [] = 0
--sumaR f (x:xs) = f x + sumaR f xs
--Ejercicio 6.14.2. Definir, por plegado, la funci�n
--sumaP :: Num b => (a -> b) -> [a] -> b
--tal que (suma f xs) es la suma de los valores obtenido aplicando la funci�n f a lo elementos de
--la lista xs. Por ejemplo,
--sumaP (*2) [3,5,10] == 36
--sumaP (/10) [3,5,10] == 1.8
--Soluci�n:
--sumaP :: Num b => (a -> b) -> [a] -> b
--sumaP f = foldr (\x y -> (f x) + y) 0
--132 Cap�tulo 6. Funciones de orden superior
--6.15. Redefinici�n de la funci�n map usando foldr
--Ejercicio 6.15.1. Redefinir, por recursi�n, la funci�n map. Por ejemplo,
--mapR (+2) [1,7,3] == [3,9,5]
--Soluci�n:
--mapR :: (a -> b) -> [a] -> [b]
--mapR f [] = []
--mapR f (x:xs) = f x : mapR f xs
--Ejercicio 6.15.2. Redefinir, usando foldr, la funci�n map. Por ejemplo,
--mapP (+2) [1,7,3] == [3,9,5]
-

--Soluci�n:
--mapP :: (a -> b) -> [a] -> [b]
--mapP f = foldr g []
--where g x xs = f x : xs
--La definici�n por plegado usando lambda es
--mapP' :: (a -> b) -> [a] -> [b]
--mapP' f = foldr (\x y -> f x:y) []
--Otra definici�n es
--mapP'' :: (a -> b) -> [a] -> [b]
--mapP'' f = foldr ((:) . f) []
--6.16. Redefinici�n de la funci�n filter usando foldr
--Ejercicio 6.16.1. Redefinir, por recursi�n, la funci�n filter. Por ejemplo,
--filterR (<4) [1,7,3,2] => [1,3,2]
--Soluci�n:
--filterR :: (a -> Bool) -> [a] -> [a]
--filterR p [] = []
--filterR p (x:xs) | p x = x : filterR p xs
-- | otherwise = filterR p xs
--6.17. Suma de las sumas de las listas de una lista de listas 133
--Ejercicio 6.16.2. Redefinir, usando foldr, la funci�n filter. Por ejemplo,
--filterP (<4) [1,7,3,2] => [1,3,2]
--Soluci�n:
--filterP :: (a -> Bool) -> [a] -> [a]
--filterP p = foldr g []
--where g x y | p x = x:y
-- | otherwise = y
--La definici�n por plegado y lambda es
--filterP' :: (a -> Bool) -> [a] -> [a]
--filterP' p = foldr (\x y -> if (p x) then (x:y) else y) []
--6.17. Suma de las sumas de las listas de una lista de listas
--Ejercicio 6.17.1. Definir, mediante recursi�n, la funci�n
--sumllR :: Num a => [[a]] -> a
--tal que (sumllR xss) es la suma de las sumas de las listas de xss. Por ejemplo,
--sumllR [[1,3],[2,5]] == 11
--Soluci�n:
--sumllR :: Num a => [[a]] -> a
--sumllR [] = 0
--sumllR (xs:xss) = sum xs + sumllR xss
--Ejercicio 6.17.2. Definir, mediante plegado, la funci�n
--sumllP :: Num a => [[a]] -> a
--tal que (sumllP xss) es la suma de las sumas de las listas de xss. Por ejemplo,
--sumllP [[1,3],[2,5]] == 11
--Soluci�n:
--sumllP :: Num a => [[a]] -> a
--sumllP = foldr f 0
--where f xs n = sum xs + n
--134 Cap�tulo 6. Funciones de orden superior
--La definici�n anterior puede simplificarse usando lambda
--sumllP' :: Num a => [[a]] -> a
--sumllP' = foldr (\xs n -> sum xs + n) 0
--Ejercicio 6.17.3. Definir, mediante recursi�n con acumulador, la funci�n
--sumllA :: Num a => [[a]] -> a
--tal que (sumllA xss) es la suma de las sumas de las listas de xss. Por ejemplo,
--sumllA [[1,3],[2,5]] == 11
--Soluci�n:
--sumllA :: Num a => [[a]] -> a
--sumllA xs = aux 0 xs
--where aux a [] = a
--aux a (xs:xss) = aux (a + sum xs) xss
--Ejercicio 6.17.4. Definir, mediante plegado con foldl, la funci�n
--sumllAP :: Num a => [[a]] -> a
--tal que (sumllAP xss) es la suma de las sumas de las listas de xss. Por ejemplo,
--sumllAP [[1,3],[2,5]] == 11
--Soluci�n:
--sumllAP :: Num a => [[a]] -> a
--sumllAP = foldl (\a xs -> a + sum xs) 0
--6.18. Lista obtenida borrando las ocurrencias de un elemento
--Ejercicio 6.18.1. Definir, mediante recursi�n, la funci�n
--borraR :: Eq a => a -> a -> [a]
--tal que (borraR y xs) es la lista obtenida borrando las ocurrencias de y en xs. Por ejemplo,
--borraR 5 [2,3,5,6] == [2,3,6]
--borraR 5 [2,3,5,6,5] == [2,3,6]
--borraR 7 [2,3,5,6,5] == [2,3,5,6,5]
--6.19. Diferencia de dos listas 135
--Soluci�n:
--borraR :: Eq a => a -> [a] -> [a]
--borraR z [] = []
--borraR z (x:xs) | z == x = borraR z xs
-- | otherwise = x : borraR z xs
--Ejercicio 6.18.2. Definir, mediante plegado, la funci�n
--borraP :: Eq a => a -> a -> [a]
--tal que (borraP y xs) es la lista obtenida borrando las ocurrencias de y en xs. Por ejemplo,
--borraP 5 [2,3,5,6] == [2,3,6]
--borraP 5 [2,3,5,6,5] == [2,3,6]
--borraP 7 [2,3,5,6,5] == [2,3,5,6,5]
--Soluci�n:
--borraP :: Eq a => a -> [a] -> [a]
--borraP z = foldr f []
--where f x y | z == x = y
-- | otherwise = x:y
--La definici�n por plegado con lambda es es
--borraP' :: Eq a => a -> [a] -> [a]
--borraP' z = foldr (\x y -> if z==x then y else x:y) []
--6.19. Diferencia de dos listas
--Ejercicio 6.19.1. Definir, mediante recursi�n, la funci�n
--diferenciaR :: Eq a => [a] -> [a] -> [a]
--tal que (diferenciaR xs ys) es la diferencia del conjunto xs e ys; es decir el conjunto de los
--elementos de xs que no pertenecen a ys. Por ejemplo,
--diferenciaR [2,3,5,6] [5,2,7] == [3,6]
--Soluci�n:
--136 Cap�tulo 6. Funciones de orden superior
--diferenciaR :: Eq a => [a] -> [a] -> [a]
--diferenciaR xs ys = aux xs xs ys
--where aux a xs [] = a
--aux a xs (y:ys) = aux (borraR y a) xs ys
--La definici�n, para aproximarse al patr�n de plegado, se puede escribir como
--diferenciaR' :: Eq a => [a] -> [a] -> [a]
--diferenciaR' xs ys = aux xs xs ys
--where aux a xs [] = a
--aux a xs (y:ys) = aux (flip borraR a y) xs ys
--Ejercicio 6.19.2. Definir, mediante plegado con foldl, la funci�n
--diferenciaP :: Eq a => [a] -> [a] -> [a]
--tal que (diferenciaP xs ys) es la diferencia del conjunto xs e ys; es decir el conjunto de los
--elementos de xs que no pertenecen a ys. Por ejemplo,
--diferenciaP [2,3,5,6] [5,2,7] == [3,6]
--Soluci�n:
--diferenciaP :: Eq a => [a] -> [a] -> [a]
--diferenciaP xs ys = foldl (flip borraR) xs ys
--La definici�n anterior puede simplificarse a
--diferenciaP' :: Eq a => [a] -> [a] -> [a]
--diferenciaP' = foldl (flip borraR)
--6.20. Producto de los n�meros que verifican una propiedad
--Ejercicio 6.20.1. Definir mediante plegado la funci�n
--producto :: Num a => [a] -> a
--tal que (producto xs) es el producto de los elementos de la lista xs. Por ejemplo,
--producto [2,1,-3,4,5,-6] == 720
--6.21. Las cabezas y las colas de una lista 137
--Soluci�n:
--producto :: Num a => [a] -> a
--producto = foldr (*) 1
--Ejercicio 6.20.2. Definir mediante plegado la funci�n
--productoPred :: Num a => (a -> Bool) -> [a] -> a
--tal que (productoPred p xs) es el producto de los elementos de la lista xs que verifican el
--predicado p. Por ejemplo,
--productoPred even [2,1,-3,4,-5,6] == 48
--Soluci�n:
--productoPred :: Num a => (a -> Bool) -> [a] -> a
--productoPred p = foldr (\x y -> if p x then x*y else y) 1
--Ejercicio 6.20.3. Definir la funci�n
--productoPos :: (Num a, Ord a) => [a] -> a
--tal que (productoPos xs) esel producto de los elementos estr�ctamente positivos de la lista xs.
--Por ejemplo,
--productoPos [2,1,-3,4,-5,6] == 48
--Soluci�n:
--productoPos :: (Num a, Ord a) => [a] -> a
--productoPos = productoPred (>0)
--6.21. Las cabezas y las colas de una lista
--Ejercicio 6.21.1. Se denomina cola de una lista xs a una sublista no vac�a de xs formada
--por un elemento y los siguientes hasta el final. Por ejemplo, [3,4,5] es una cola de la lista
--[1,2,3,4,5].
--Definir la funci�n
--colas :: [a] -> [[a]]
--tal que (colas xs) es la lista de las colas de la lista xs. Por ejemplo,
--138 Cap�tulo 6. Funciones de orden superior
--colas [] == [[]]
--colas [1,2] == [[1,2],[2],[]]
--colas [4,1,2,5] == [[4,1,2,5],[1,2,5],[2,5],[5],[]]
--Soluci�n:
--colas :: [a] -> [[a]]
--colas [] = [[]]
--colas (x:xs) = (x:xs) : colas xs
--Ejercicio 6.21.2. Comprobar con QuickCheck que las funciones colas y tails son equivalentes.
--Soluci�n: La propiedad es
--prop_colas :: [Int] -> Bool
--prop_colas xs = colas xs == tails xs
--La comprobaci�n es
--ghci> quickCheck prop_colas
-- +++ OK, passed 100 tests.
--Ejercicio 6.21.3. Se denomina cabeza de una lista xs a una sublista no vac�a de xs formada
--por el primer elemento y los siguientes hasta uno dado. Por ejemplo, [1,2,3] es una cabeza de
--[1,2,3,4,5].
--Definir, por recursi�n, la funci�n
--cabezas :: [a] -> [[a]]
--tal que (cabezas xs) es la lista de las cabezas de la lista xs. Por ejemplo,
--cabezas [] == [[]]
--cabezas [1,4] == [[],[1],[1,4]]
--cabezas [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]
--Soluci�n:
--cabezas :: [a] -> [[a]]
--cabezas [] = [[]]
--cabezas (x:xs) = [] : [x:ys | ys <- cabezas xs]
--Ejercicio 6.21.4. Definir, por plegado, la funci�n
--cabezasP :: [a] -> [[a]]
--6.21. Las cabezas y las colas de una lista 139
--tal que (cabezasP xs) es la lista de las cabezasP de la lista xs. Por ejemplo,
--cabezasP [] == [[]]
--cabezasP [1,4] == [[],[1],[1,4]]
--cabezasP [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]
--Soluci�n:
--cabezasP :: [a] -> [[a]]
--cabezasP = foldr (\x y -> [x]:[x:ys | ys <- y]) []
--Ejercicio 6.21.5. Definir, mediante funciones de orden superior, la funci�n
--cabezasS :: [a] -> [[a]]
--tal que (cabezasS xs) es la lista de las cabezasS de la lista xs. Por ejemplo,
--cabezasS [] == [[]]
--cabezasS [1,4] == [[],[1],[1,4]]
--cabezasS [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]
--Soluci�n:
--cabezasS :: [a] -> [[a]]
--cabezasS xs = reverse (map reverse (colas (reverse xs)))
--La anterior definici�n puede escribirse sin argumentos como
--cabezasS' :: [a] -> [[a]]
--cabezasS' = reverse . map reverse . (colas . reverse)
--Ejercicio 6.21.6. Comprobar con QuickCheck que las funciones cabezas y inits son equivalentes.
--Soluci�n: La propiedad es
--prop_cabezas :: [Int] -> Bool
--prop_cabezas xs = cabezas xs == inits xs
--La comprobaci�n es
--ghci> quickCheck prop_cabezas
-- +++ OK, passed 100 tests.
-- Nota. Un caso de estudio para las funciones de orden superior es el cap�tulo 16 �Codificaci�n
--  y transmisi�n de mensajes� (p�gina 331)
