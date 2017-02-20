
module Ejercicios.Ejercicios4 where

--Listado de ejercicios para poner en pr�ctica los conocimientos adquiridos sobre definici�n de tipos sin�nimos y nuevos tipos,
-- tipos recursivos y tipos recursivos polim�rficos. Y tambi�n sobre el manejo de clases de tipos en Haskell.
--Ejercicios:
--a) Definir una funci�n que dado un d�a de la semana, indique si �ste es o no laborable. Para representar 
--   el d�a de la semana se deber� crear un nuevo tipo enumerado.

data Dia=Lunes|Martes|Miercoles|Jueves|Viernes 

laborable::dia->bool
laborable d = if (d=="Lunes") True else False 

--b) Se quiere ordenar los elementos de una lista (cuyos elementos son comparables) mediante el algoritmo del quicksort.

--c) Se pide implementar una funci�n que dada un n�mero (de cualquier tipo que soporte la operaci�n de divisi�n) 
--   y una lista de n�meros del mismo tipo, divida a ese n�mero por cada uno de los elementos contenidos 
--   en la lista y devuelva una lista con el resultado.


--Ejemplos de aplicaci�n de la funci�n son:
-- > divisiones 5 [1,2,3]
--[Just 5,Just 2,Just 1]
-- > divisiones 5 [1,2,3,0,9,10]
--[Just 5,Just 2,Just 1,Nothing,Just 0,Just 0]

--d) Dado un nuevo tipo de datos para representar un �rbol binario de cualquier tipo, definido como sigue:
--data Arbol a = AV | Rama (Arbol a) a (Arbol a)
--Se pide definir una funci�n que visualice el �rbol por pantalla de una determinada forma: separando cada hijo izquierdo y derecho por �|�, la ra�z entre guiones y cada nivel diferente del �rbol por �( )�. Ejemplos de aplicaci�n de la funci�n ser�a los siguientes:
-- > mostrarArbol (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
--"((60)|-8-|())|-5-|(4)"
-- > mostrarArbol (Rama AV 5 (Rama AV 4 AV))
--"()|-5-|(4)"
--�Ser�a equivalente a declarar el nuevo tipo de datos Arbol como una instancia de la clase Show?
--data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show
---e) Se quiere poder mostrar por pantalla los datos de los estudiantes matriculados en una universidad que pertenezcan a alguna de las asociaciones de �sta (culturales, deportivas,

--de representaci�n estudiantil, etc.). Para ello se deber�n crear nuevos tipos de datos que representen:
--Estudiante, de cada uno se debe disponer del nombre y titulaci�n
 --Titulaci�n, que pueden ser tres: Grado II, Grado II_ADE, Grado ADE
--Lista de estudiantes matriculadosLista de estudiantes que pertenecen a asociaciones
--Un ejemplo de aplicaci�n de la funci�n que se pide podr�a ser:
-- > mostrarAlumnosAsociaciones(listaMatriculados,listaAsociaciones)
--"(Carlos Calle,GradoADE_II)(Irene Plaza,GradoADE)"
--Donde Carlos Calle e Irene Plaza son los �nicos estudiantes matriculados que pertenecen a alg�n tipo de asociaci�n en la universidad.
--f) Se quiere poder representar una fecha de la siguiente forma: dd/mm/aaaa, para ello se deber� crear un nuevo tipo de datos en Haskell. Por ejemplo, si se crea un nuevo tipo de datos cuyo constructor de datos es Fecha, en el int�rprete al poner fechas concretas nos devolver�a la representaci�n de la fecha que hayamos definido:
-- > Fecha 10 10 2013 > Fecha 24 12 2012
--10/10/2013 24/12/2012
--g) Teniendo en cuenta el nuevo tipo de datos Fecha definido anteriormente, se pide una funci�n que sea capaz de comparar dos fechas. Ejemplos de aplicaci�n de la funci�n ser�an:
-- > mismaFecha (Fecha 10 10 2013) (Fecha 10 10 2013)
--True
-- > mismaFecha (Fecha 10 11 2013) (Fecha 10 10 2013)
--False
--h) Teniendo en cuenta la definici�n de la funci�n qs del apartado (b) de este listado de ejercicios, se pide ordenar una lista de fechas mediante quicksort. Ejemplos de aplicaci�n de la funci�n ser�an:
-- > qs [(Fecha 10 10 2013), (Fecha 24 12 2012), (Fecha 10 09 2013), (Fecha 12 12 2013)]
--[24/12/2012,10/9/2013,10/10/2013,12/12/2013]
--i) Se pide crear una nueva clase de tipos, llamada Coleccion, para representar colecciones de datos de cualquier tipo, donde los tipos pertenecientes a esta clase tendr�n el siguiente comportamiento:

--esVacia: funci�n para saber si la colecci�n est� vac�a.
--insertar: insertar� un nuevo elemento en la colecci�n.
--primero: devolver� el primer elemento de la colecci�n.
--eliminar: eliminar� un elemento de la colecci�n.
--size: devolver� el n�mero de elementos de la colecci�n.
--Algunas de las funciones anteriores variar�n su implementaci�n en funci�n del tipo de colecci�n particular que sea instancia de la clase Coleccion. Por ello, se pide crear dos instancias diferentes de esta clase para los dos nuevos tipos de datos que se presentan a continuaci�n:
--data Pila a = Pil [a] deriving Show
--data Cola a = Col [a] deriving Show
--El primero de ellos representa una estructura de datos LIFO con elementos de tipo a. El segundo representa una estructura de datos FIFO de elementos de tipo a.
--Ejemplos de aplicaci�n de las funciones para ambos tipos de datos ser�an:
-- > insertar 10 (Col [1,2,3,4])
--Col [1,2,3,4,10]
-- > insertar 10 (Pil [1,2,3,4])
--Pil [1,2,3,4,10]
-- > primero (Col [1,2,3,4,10])
--1
-- > primero (Pil [1,2,3,4,10])
--10
-- > eliminar (Col [1,2,3,4,10])
--Col [2,3,4,10]
-- > eliminar (Pil [1,2,3,4,10])
--Pil [1,2,3,4]