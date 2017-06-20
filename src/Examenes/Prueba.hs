
module Examenes.Prueba where
-- Ejercicio 2) (2 puntos)
-- a) Implementar una función polimórfica filter' en Haskell que dada una lista y una función que recibe
-- un elemento de la lista y devuelve un valor booleano, devuelva como resultado una lista que contenga
-- exclusivamente los elementos de la lista original para los que la función pasada devolvió True.
-- La función filter' debe definirse de forma polimórfica y su implementación debe realizarse
-- utilizando la función foldl. Ejemplos de aplicación de la función son los siguientes:
-- > filter' odd [1,2,3,4)
-- [l, 3]
-- > filter' isUpper ['A', 'b', 'C', 'd']
-- "AC"
-- b) Para realizar tratamientos avanzados de datos se quiere poder aplicar un conjunto de funciones sobre
-- una lista de elementos de cualquier tipo. Las transformaciones serán siempre del tipo de datos de los
-- elementos de la lista, es decir, si tenemos una lista de String, sólo se pueden hacer transformaciones
-- que devuelvan string. Se pide implementar una función que sea capaz de hacer lo descrito
-- anteriormente. Un ejemplo de aplicación de la función es el siguiente:
-- > mmap [\n->tail n, \n->map toUpper n] ["Hola", "Adios", "caracola"]
-- ["OLA" I "DIOS" I "ARACOLA"]
-- Ejercicio 3) (2 puntos)
-- Implementar una función que dada una lista de elementos de cualquier tipo, sea capaz de crear sublistas con
--todos los elementos repetidos. ·
-- Ejemplos de aplicación de la función son:
-- >iguales' [l,2,3,1,5,3,2,6,5,7,8,7)
-- [[8] I [7,7) t [6], [5,5], [3,3], [2,2], [1 , 1)]
-- > igual es' [True,False ,False,True,False]
-- [[False, False, False] , [True, True] J
-- > iguales' [(1,2), (3,5), (1,2), (10,20), (3,5))
-- [ [ ( 1 o f .2 o ) l t [ ( 3 t 5) t ( 3 t 5 ) l t [ ( l , 2 ) t ( l , 2 ) ] l
-- > iguales ' [ 'a' ' 'b' ' ' b' ' 'c' l
-- [ "e TI I 11 bb" I "a" ]
--1

--u Universidad
--Rey Juan Carlos
--Programación Declarativa - Prueba Programación Funcional
--2 de Noviembre de 2015
--APELLIDOS;
--TITULACIÓN:
--Ejercicio 4) (3 puntos)
--Se quiere definir una librería genérica para determinar precios de productos en base al precio base y al
--impuesto aplicable (IV A). Dado que la librería debe ser genérica debe soportar cualquier tipo de datos, de
--manera que los datos para los que se quiera calcular un precio con impuestos deberán ser instancias de las
--clases que se definan en la librería.
--Se pide lo siguiente:
--Definir nuevos tipos de datos para representar dos tipos de productos diferentes:
--o Música: se le aplicará un 21 % de IV A.
--o- Libros: se le aplicará un 15% de IV A.
--Cada tipo de datos además de su IV A debe contener su precio base.
--Definir las clases de tipos necesarias para soportar el cálculo de impuestos, con independencia del tipo
--de datos del que se trate. Debe tener las funciones necesarias para obtener el precio base de un producto
--de cualquier tipo, su impuesto y cuál seria la tasa aplicable según su precio base e impuesto.
--Implementar una función pvp que dado un elemento de cualquier tipo, sea capaz de devolver su precio
--final una vez aplicado el IV A.
--2

-- declaracion del tpo 
data Dat=D{num1::Integer,num2::Integer}
-- definicion de la instancia
instance Eq Dat where
	(==)(D n11 n12)(D n21 n22)= (n11==n21)&&(n12==n22)
	(/=)(D n11 n12)(D n21 n22)= (n11/=n21)&&(n12/=n22)
	
instance Ord Dat where
	(<)(D n1 n2)(D n3 n4)= (n1<n2)&&(n3<n4)
	(<=)(D n1 n2)(D n3 n4)= (n1<=n2)&&(n3<n4)||(n1<n2)&&(n3<=n4)||(n1<=n2)&&(n3<=n4)
	
instance Show Dat where
	show (D n1 n2)= "numero1= "++show n1++" numero2="++show n2


-- prueba en programa
ver::Dat->Dat->Dat
ver (D n1 n2)(D n3 n4) = if ((D n1 n2)<=(D n3 n4))then (D n1 n2) else(D n3 n4)

