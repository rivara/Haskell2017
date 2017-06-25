
module Examenes.Extraordinaria2013 where
--2) Se pide implementar un programa en Haskell que sea capaz de obtener el número de errores de una tabla de multiplicar, junto con los elementos de la tabla con los errores. Por ejemplo, si recibe la siguiente tabla del 1 el resultado debería de ser algo como:
--  "Hay 0 errores, que son: ".
--1 × 0 = 0
--1 × 1 = 1
--1 × 2 = 2
--1 × 3 = 3
--1 × 4 = 4
--1 × 5 = 5
--1 × 6 = 6
--1 × 7 = 7
--1 × 8 = 8
--1 × 9 = 9
--1 × 10 = 10
--Sin embargo si recibe la siguiente tabla del 3 el resultado debería de ser algo como:
--Hay 4 errores, que son: 4x1=3, 3x3=10, 3x6=20, 3x3=24".
--3 × 0 = 0 4 × 1 = 3
--3 × 2 = 6 3 × 3 = 10
--3 × 4 = 12
--3 × 5 = 15 3 × 6 = 20
--3 × 7 = 21 3 × 3 = 24
--3 × 9 = 27
--3 × 10 = 30




--4) Se pide implementar un programa en Haskell que sea capaz de obtener las estadísticas de los resultados de un equipo de fútbol en una temporada determinada. Debe ser lo suficientemente general como para que se pueda aplicar a diferentes temporadas y en diferentes ligas.
--Una temporada se compone de diferentes jornadas, cada jornada se compone de una serie de encuentros y cada encuentro contiene información de los dos equipos que y del resultado final del partido, como se puede ver en la Figura 1.
--Figura 1. Ejemplo de las dos primeras jornadas de la liga de la temporada 2010/2011
--Hoja de Ejercicios 5
--Programación Declarativa
--4
--Un ejemplo podría ser una liga con 4 equipos (R. Madrid, Valencia, Betis y Atlético de Madrid), donde los resultados de las 3 únicas jornadas de la temporada pasada fueron:
--Jornada 1:
--R. Madrid – Betis (2-1)
--Atlético de Madrid – Valencia (6,1)
--Jornada 2:
--Valencia – R. Madrid (1,5)
--Betis – Atlético de Madrid (3,3)
--Jornada 3:
--Betis –Valencia (0,0)
--R. Madrid – Atlético de Madrid (3,3)
-- Si se quieren conocer las estadísticas del R. Madrid para dicha temporada un ejemplo de aplicación de función podría ser el siguiente:--
-- > estadisticas(madrid,temporada2010_2011)
--(Ganados: 2, Empatados: 1, Perdidos: 0)
--Se valorará positivamente la claridad y extensibilidad del código, así como la definición de tipos de datos adecuados para la resolución del problema.
--5) Una característica interesante de los árboles binarios es que partiendo de un recorrido en orden y de un recorrido en preorden se puede reconstruir el árbol de manera unívoca. Crear una función, recEn_PreOrden, que dadas dos listas, una del recorrido en orden de un árbol binario y otra del recorrido en preorden, devuelva el árbol correspondiente. El tipo de dato a utilizar será:
--data Arbol = AV | Nodo Arbol Int Arbol deriving Show
