
module Ejercicios.Ejercicios3 where

--a) Se pide una función que dada una lista de racionales, donde cada racional se define como dos números enteros (numerador y denominador), 
-- y un número racional, devuelva otra lista con todos los racionales equivalentes al dado. Realiza dos versiones del ejercicio:
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


type Numerador =Integer
type Denominador =Integer
type Racional =(Numerador,Denominador)

equivalentes::[Racional]->Racional->[Racional]
equivalentes = equivalentesAux []

equivalentesAux :: [Racional]->[Racional]-> Racional ->[Racional]
equivalentesAux r [] _= r
equivalentesAux r (ra:ras)rae= equivalentesAux(if sonEquivalente rae ra then r++[ra] else r)  ras rae

sonEquivalente::Racional->Racional->Bool
sonEquivalente (n1,d1)(n2,d2)= n1`rem`n2==0 && d1`rem`d2==0


data Racional'= R (Numerador ,Denominador)


equivalentes'::[Racional]->Racional->[Racional]
equivalentes' = equivalentesAux' []

equivalentesAux' :: [Racional]->[Racional]-> Racional ->[Racional]
equivalentesAux' r [] _= r
equivalentesAux' r (ra:ras) rae= equivalentesAux(if sonEquivalente rae ra then r++[ra] else r)  ras rae

sonEquivalente'::Racional'->Racional'->Bool
sonEquivalente' (R(n1,d1)) (R(n2,d2))= n1`rem`n2==0 && d1`rem`d2==0


data Racional''= Ra{num::Integer,den::Integer}

sonEquivalente''::Racional''->Racional''->Bool
sonEquivalente'' (Ra n1 d1) (Ra n2 d2)= n1`rem`n2==0 && d1`rem`d2==0	


sonEquivalente'''::Racional''->Racional''->Bool
sonEquivalente''' ra1 ra2= (num ra1) `rem`(num ra2)==0 && (den ra1)`rem`(den ra2)==0	


--b) Se pide varias funciones para hacer lo siguiente:
--1. Función que dado un punto de coordenadas y una dirección (Norte, Sur, Este u Oeste) 
--mueva el punto hacia la dirección indicada. Un ejemplo de aplicación de la función sería:
-- > mover Este (3,4) > mover Norte (3.5,9.2)
--(4,4) (3.5,10.2)

data Direccion= Norte | Sur | Este| Oeste deriving Show
type PuntoCardinal= (Float,Float)

mover::Direccion->PuntoCardinal->PuntoCardinal
mover Norte (x,y)= (x,y+1)
mover Sur (x,y)= (x,y-1)
mover Este (x,y)= (x+1,y)
mover Oeste (x,y)=(x-1,y)

data PuntoCardinal'= PC {x::Float, y::Float} deriving Show

mover' Norte (PC x y)= (PC x (y+1))
mover' Sur (PC x y)= (PC x (y+1))
mover' Este (PC x y)= (PC (x+1) y)
mover' Oeste (PC x y)= (PC (x-1) y)



--2. Función que dados dos puntos de coordenadas indique cuál está más al sur. Ejemplos de aplicación de la función son:
-- > masAlSur (3,5) (4,6) > masAlSur (4.5,-6.2) (4.5,-7)
--(3.0,5.0) (4.5,-7.0)

masAlSur:: PuntoCardinal'-> PuntoCardinal' -> PuntoCardinal'
masAlSur pc1 pc2= if (y pc1)<(y pc2) then pc1  else pc2 

--3. Función que calcule la distancia entre dos puntos:
-- > distancia (3,5) (6,7)
--3.6055512
-- raizCuadrada((x2-x1)^2+(y2-y1)^2)
distancia::PuntoCardinal' ->PuntoCardinal' -> Float
distancia (PC x1 y1) (PC x2 y2) = sqrt((x2-x1)^2 + (y2-y1)^2)


--4. Función que dado un punto y una lista de direcciones, retorne el camino que forman todos los puntos después 
-- de cada movimiento sucesivo desde el punto original:
-- >camino (3.2,5.5) [Sur,Este,Este,Norte,Oeste]
--[(3.2,4.5),(4.2,4.5),(5.2,4.5),(5.2,5.5),(4.2,5.5)]

camino::PuntoCardinal'->[Direccion]->[PuntoCardinal']
camino = caminoAux []

caminoAux:: [PuntoCardinal'] -> PuntoCardinal' -> [Direccion] -> [PuntoCardinal']
caminoAux r _ [] = r
caminoAux r pc (d:ds) = caminoAux (r ++ [pf]) pf ds
  where
    pf = mover' d pc


--c) La empresa RealTimeSolutions, Inc. está trabajando en un controlador para una central domótica.
--   El controlador recibe información de termostatos situados en diferentes habitaciones de la vivienda y basándose en esta información, 
--   activa o desactiva el aire acondicionado en cada una de las habitaciones. Los termostatos pueden enviar la información sobre la temperatura 
--   en grados Celsius o Fahrenheit.
--   A su vez, los aparatos de aire acondicionado reciben dos tipos de órdenes: apagar y encender (on y off). Se pide:


--1. Definir un tipo de datos para representar las temperaturas en ambos tipos de unidades.

type Celsius = Float
type Fahrenheit = Float

data Temperatura= Celsius' Float|Fahrenheit' Float deriving Show

--2. Definir una función convert que dada una temperatura en grados Celsius la convierta a grados Fahrenheit y viceversa. 
--(Conversión de C a F: f = c * 9/5 + 32; conversión de F a C: c = (f – 32) * 5/9.)

-- type
convertCtoF::Celsius->Fahrenheit
convertCtoF c=c * (9 / 5) + 32

convertFtoC::Fahrenheit->Celsius
convertFtoC f= f - 32 * (5 / 9)

-- data
convertCtoF'::Temperatura->Temperatura
convertCtoF'(Celsius' c)=(Fahrenheit' (c * (9 / 5) + 32))

convertFtoC'::Temperatura->Temperatura
convertFtoC'(Fahrenheit' f)= (Celsius'(f - 32 * (5 / 9)))


--3. Definir un tipo de datos para representar las órdenes a los aparatos de a/a.
data OrdenAA = On | Off deriving Show

--4. Definir una función action que dada una temperatura en cierta habitación determine la acción a realizar 
--sobre el aparato de a/a de dicha habitación. El controlador debe encender el aparato si la temperatura excede de 28ºC. 
-- Ejemplos de aplicación:
-- > action(Celsius(25)) > action(Fahrenheit(83.5))
--On Off
action::Temperatura-> OrdenAA
action (Celsius' c)= if c<28 then On else Off
action (Fahrenheit' f) = action (convertCtoF' (Fahrenheit' f))



--d) Definir un tipo moneda para representar euros y dólares USA. Definir una función que convierta
-- entre ambas monedas sabiendo que el factor de conversión de euros a dólares es 1.14.
data Moneda= Euros' Float| Dollar' Float deriving Show

conver::Moneda->Moneda
conver (Euros' e) = (Dollar' (e * 1.14))
conver (Dollar' d) = (Euros' (d / 1.14))
-- conver (Dollar' 3.3)


--e) Dada el siguiente tipo de datos recursivo que representa expresiones aritméticas:
data Expr = Valor Integer
 |Expr :+: Expr
 |Expr :-: Expr
 |Expr :*: Expr deriving Show



--e.1) Se pide una función para calcular el valor de una expresión.
calcularExp::Expr->Integer
calcularExp(Valor v)=v
calcularExp(expr1:+:expr2)=(calcularExp expr1) + (calcularExp expr2)
calcularExp(expr1:-:expr2)=(calcularExp expr1) - (calcularExp expr2)
calcularExp(expr1:*:expr2)=(calcularExp expr1) * (calcularExp expr2)
-- calcularExp((Valor 3:+: Valor 4):-:Valor 1)


--e.2) Se pide una función para calcular el número de constantes de una expresión.

calcularConstante::Expr->Integer
calcularConstante(Valor v)=0
calcularConstante(expr1:+:expr2)=1+(calcularConstante expr1)+(calcularConstante expr2)
calcularConstantes(expr1:-:expr2)=1+(calcularConstante expr1)-(calcularConstante expr2)
calcularConstantes(expr1:*:expr2)=1+(calcularConstante expr1)*(calcularConstante expr2)
--calcularConstantes((Valor 3:+: Valor 4):-:Valor 1)

--f) Dado el siguiente tipo de datos que representa un árbol binario:
data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show
--Se pide definir una función que calcule el espejo de un árbol.
--Ejemplos de aplicación de la función serían:
-- > espejo (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
--Rama (Rama AV 4 AV) 5 (Rama AV 8 (Rama AV 60 AV))
-- > espejo (Rama AV 5 (Rama AV4 AV))
--Rama (Rama AV 4 AV) 5 AV

espejo:: Arbol a->Arbol a
espejo AV=AV
espejo (Rama AV r AV)= Rama AV r AV
espejo (Rama i r d)= (Rama ed r ei)
					where ei= espejo i;ed= espejo d


