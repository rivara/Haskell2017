module Ejercicios.Ejercicios4 where
import Data.Maybe
--PROBLEMA INSTANCIAR LA CLASE ORD
--Listado de ejercicios para poner en pr�ctica los conocimientos adquiridos sobre definici�n de tipos sin�nimos y nuevos tipos,
-- tipos recursivos y tipos recursivos polim�rficos. Y tambi�n sobre el manejo de clases de tipos en Haskell.
--Ejercicios:
--a) Definir una funci�n que dado un d�a de la semana, indique si �ste es o no laborable. Para representar 
--   el d�a de la semana se deber� crear un nuevo tipo enumerado.

data Dia=Lunes|Martes|Miercoles|Jueves|Viernes deriving (Show,Eq)

laborable::[Dia]
laborable = [Lunes,Martes,Miercoles,Jueves,Viernes]

pertenece:: (Eq a)=>[a]->a->Bool
pertenece [] _= False
pertenece (x:xs) e= x==e || pertenece xs e

esLaborable ::Dia->Bool
esLaborable d= d`elem`laborable		

--b) Se quiere ordenar los elementos de una lista (cuyos elementos son comparables) mediante el algoritmo del quicksort.
quicksort::(Ord a)=>[a]->[a]
quicksort[]=[]
quicksort (e:es) = quicksort ordenI ++[e]++ quicksort ordenD 
 where 
	ordenI=[n|n<-es,n<e]
	ordenD=[n|n<-es,n>=e]

--c) Se pide implementar una funci�n que dada un n�mero (de cualquier tipo que soporte la operaci�n de divisi�n) 
--   y una lista de n�meros del mismo tipo, divida a ese n�mero por cada uno de los elementos contenidos 
--   en la lista y devuelva una lista con el resultado.
--Ejemplos de aplicaci�n de la funci�n son:
-- > divisiones 5 [1,2,3]
--[Just 5,Just 2,Just 1]
-- > divisiones 5 [1,2,3,0,9,10]
--[Just 5,Just 2,Just 1,Nothing,Just 0,Just 0]

divisiones :: Integral a=> a -> [a]->[Maybe a]
divisiones e es = foldl(\a b ->if b==0 then a ++[Nothing] else a ++[Just(e`div`b)]) [] es





--d) Dado un nuevo tipo de datos para representar un �rbol binario de cualquier tipo, definido como sigue:
--data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show
--Se pide definir una funci�n que visualice el �rbol por pantalla de una determinada forma: separando cada hijo izquierdo y derecho por �|�, la ra�z entre guiones y cada nivel diferente del �rbol por �( )�. Ejemplos de aplicaci�n de la funci�n ser�a los siguientes:
-- > mostrarArbol (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
--"((60)|-8- |())|-5- |(4)"
-- > mostrarArbol (Rama AV 5 (Rama AV 4 AV))
--"()|-5- |(4)"
--�Ser�a equivalente a declarar el nuevo tipo de datos Arbol como una instancia de la clase Show?
data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

mostrarArbol:: Show a=>Arbol a->String
mostrarArbol (AV)="()"
mostrarArbol(Rama hizq r hder)="(" ++(mostrarArbol hizq)++ "|-" ++ show r  ++ "- |" ++ (mostrarArbol hizq) 



---e) Se quiere poder mostrar por pantalla los datos de los estudiantes matriculados en una universidad que pertenezcan
--  a alguna de las asociaciones de �sta (culturales, deportivas,
--de representaci�n estudiantil, etc.). Para ello se deber�n crear nuevos tipos de datos que representen:
--Estudiante, de cada uno se debe disponer del nombre y titulaci�n
 --Titulaci�n, que pueden ser tres: Grado II, Grado II_ADE, Grado ADE
--Lista de estudiantes matriculadosLista de estudiantes que pertenecen a asociaciones
--Un ejemplo de aplicaci�n de la funci�n que se pide podr�a ser:
-- > mostrarAlumnosAsociaciones(listaMatriculados,listaAsociaciones)
--"(Carlos Calle,GradoADE_II)(Irene Plaza,GradoADE)"
--Donde Carlos Calle e Irene Plaza son los �nicos estudiantes matriculados que pertenecen a alg�n tipo de asociaci�n en la universidad.
---- 

--f) Se quiere poder representar una fecha de la siguiente forma: dd/mm/aaaa, para ello se deber� crear 
-- un nuevo tipo de datos en Haskell. Por ejemplo, si se crea un nuevo tipo de datos cuyo constructor de datos es Fecha, en el int�rprete 
-- al poner fechas concretas nos devolver�a la representaci�n de la fecha que hayamos definido:
-- > ver 10 10 2013

data Fecha =Fecha{dia::Integer,mes::Integer,anio::Integer}

instance Show Fecha where
 show (Fecha d m a)= show d ++"/"++ show m ++"/"++ show a

ver:: Integer->Integer->Integer ->Fecha
ver  d m a = (Fecha d m a) 


--g) Teniendo en cuenta el nuevo tipo de datos Fecha definido anteriormente, se pide una funci�n 
-- que sea capaz de comparar dos fechas. Ejemplos de aplicaci�n de la funci�n ser�an:
-- > mismaFecha (Fecha 10 10 2013) (Fecha 10 10 2013)
--True
-- > mismaFecha (Fecha' 10 11 2013) (Fecha' 10 10 2013)
--False

instance Eq Fecha where
	(==) (Fecha d1 m1 a1) (Fecha d2 m2 a2) = d1 == d2 && m1 == m2 && a1 == a2


mismaFecha::Fecha->Fecha->Bool
mismaFecha (Fecha d1 m1 a1)(Fecha d2 m2 a2)= d1==d2 && m1==m2 && a1==a2 










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

-- polimorfica
class Coleccion c where
 esVacia ::c a-> Bool
 insertar :: a-> c a -> c a
 primero:: c a ->a
 eliminar:: c a -> c a 
 size :: c a ->Int
 
data Pila a = Pil [a] deriving Show
data Cola a = Col [a] deriving Show
 
instance Coleccion Pila where
   esVacia(Pil p)= null p
   insertar e (Pil p)=(Pil(p++[e]))
   primero (Pil p) = last p
   eliminar (Pil p)= (Pil(init p))
   size (Pil p)= length p
 

instance Coleccion Cola where
  esVacia(Col c)=null c
  insertar e (Col c)=(Col([e]++c))
  primero (Col c)=head c
  eliminar (Col c)=(Col(tail c))
  size(Col c)= length c
	
-- pruebas
-- pila
-- esVacia (Pil[])
-- insertar 'g' (Pil ['a','b','c','d'])
-- primero (Pil ['a','b','c','d'])

-- cola
-- esVacia (Pil[])
-- insertar 10 (Col [1,2,3,4])
-- primero (Pil [1,2,3,4])
