
module Arboles where
--Ejercicios Árboles
--Dada la siguiente definición de tipos:
data Arbol a = AVacio | Nodo (Arbol a) a (Arbol a) deriving Show
--Ejercicio 1
--Implementad una función que devuelva qué altura tiene un árbol.
altura:: Arbol a->Int
altura AVacio= 0
altura (Nodo i _ d)= 1+ (max(altura i) (altura d))


--Ejercicio 2
--Implementad una función que devuelva el número de nodos de un árbol.
-- altura(Nodo (Nodo (Nodo AVacio 2 AVacio) 2 AVacio) 1 (Nodo AVacio 3 AVacio))
numNodos:: Arbol a ->Int
numNodos AVacio= 0
numNodos (Nodo i _ d) = 1+(altura i) + (altura d)
--Ejercicio 3
--Implementad una función que devuelva cuántas ramas tiene un árbol.
-- altura(Nodo (Nodo (Nodo AVacio 2 AVacio) 2 AVacio) 1 (Nodo AVacio 3 AVacio))
numRamas::Arbol a ->Int 
numRamas AVacio = 0
numRamas (Nodo i _ d)= (altura i) + (altura d)

--Ejercicio 4 
--Implementad una función que dado un árbol y un elemento determine si éste último
--está en una hoja.
-- estaEnHoja (Nodo (Nodo (Nodo AVacio 2 AVacio) 2 AVacio) 1 (Nodo AVacio 3 AVacio)) 2

estaEnHoja :: Eq a => Arbol a -> a -> Bool
estaEnHoja AVacio _ = False
estaEnHoja (Nodo AVacio r AVacio) e = e == r
estaEnHoja (Nodo hizq _ hder) e = (estaEnHoja hizq e) || (estaEnHoja hder e)

--Ejercicio 5 
--Implementad una función que dado un árbol devuelva una lista con los elementos que
--ocupen una hoja del mismo.
-- arrayHoja (Nodo (Nodo AVacio 2 (Nodo AVacio 3 AVacio)) 2 (Nodo AVacio 7 AVacio))

arrayHoja::Eq a =>Arbol a ->[a]
arrayHoja AVacio = []
arrayHoja (Nodo AVacio r AVacio) =[r]
arrayHoja (Nodo hizq _ hder) = (arrayHoja hizq)++(arrayHoja hder) 


--Ejercicio 6 
--Implementad una función que dado un árbol y dos números enteros, correspondientes
--a dos niveles del árbol, devuelva una lista con los elementos situados entre los dos
--niveles.
 -- elementosEntreNiveles (Nodo (Nodo AVacio 2 (Nodo AVacio 3 AVacio)) 2 (Nodo AVacio 7 AVacio)) 2 2
elementosEntreNiveles:: Arbol a -> Int -> Int -> [a]
elementosEntreNiveles = elementosEntreNivelesAux 1 

elementosEntreNivelesAux :: Int->Arbol a ->Int->Int->[a]
elementosEntreNivelesAux _ AVacio _ _=[]
elementosEntreNivelesAux na (Nodo hizq r hder) n1 n2 = if na < n1 then (elementosEntreNivelesAux ns hizq n1 n2) ++ (elementosEntreNivelesAux ns hder n1 n2) else if (na >= n1) && (na <= n2) then r:(if ns <= n2 then (elementosEntreNivelesAux ns hizq n1 n2) ++ (elementosEntreNivelesAux ns hder n1 n2) else []) else []
  where
    ns = na + 1
    										
--Ejercicio 7 
--Implementad una función que dado un árbol lo devuelva eliminando sus hojas.
-- eliminarHojas (Nodo (Nodo (Nodo AVacio 2 AVacio) 2 AVacio) 1 (Nodo AVacio 3 AVacio)) 

eliminarHojas :: Arbol a-> Arbol a
eliminarHojas AVacio = AVacio
eliminarHojas (Nodo AVacio _ AVacio)=AVacio
eliminarHojas (Nodo hizq r hder)= Nodo (eliminarHojas hizq) r (eliminarHojas hder)

--Ejercicio 8 
--Implementad una función que determine si un árbol es zurdo. Un árbol es zurdo si se
--da alguna de las siguientes condiciones:
--• Es un árbol vacío
--• Es un nodo hoja
--• Sus hijos izquierdo y derecho son zurdos y más de la mitad de sus
--descendientes están en el hijo izquierdo
-- zurdo (Nodo (Nodo (Nodo AVacio 2 AVacio) 2 AVacio) 1 (Nodo AVacio 3 AVacio)) 
zurdo ::Arbol a ->Bool
zurdo AVacio = True
zurdo (Nodo AVacio r AVacio)= True
zurdo (Nodo hizq r hder) = (zurdo hizq) && (zurdo hder) && (nodosHizq < (nodosHizq + (numNodos hder) `div` 2))
	where
		nodosHizq = numNodos hizq


--Ejercicio 9 Probar
--Implementad una función que dado un árbol devuelva el número de nodos que tienen
--vacío su hijo derecho.
--der (Nodo (Nodo (Nodo AVacio 2 AVacio) 2 AVacio) 1 (Nodo AVacio 3 AVacio)) 
der::Arbol a -> Int
der AVacio = 0
der(Nodo AVacio _ AVacio)= 0
der(Nodo AVacio _ hder)= 0+(der hder)
der(Nodo hizq _ AVacio)= 1+ (der hizq)
der(Nodo hizq _ hder)= (der hizq)+ (der hizq)



--Ejercicio 10 Probar
--Implementad una función que dado un árbol determine si es un árbol binario de
--búsqueda.
-- esABB (Nodo (Nodo (Nodo AVacio 2 AVacio) 2 AVacio) 1 (Nodo AVacio 3 AVacio)) 
esABB :: Ord a => Arbol a -> Bool
esABB AVacio = True
esABB (Nodo AVacio _ AVacio) = True
esABB (Nodo AVacio r hder) = (menorMayores hder) > r && (esABB hder)
esABB (Nodo hizq r AVacio) = (mayorMenores hizq) < r && (esABB hizq)
esABB (Nodo hizq r hder) = (mayorMenores hizq) < r && (menorMayores hder) > r && (esABB hizq) && (esABB hder)

menorMayores :: Arbol a -> a
menorMayores (Nodo AVacio r _) = r
menorMayores (Nodo hizq _ _) = menorMayores hizq

mayorMenores :: Arbol a -> a
mayorMenores (Nodo _ r AVacio) = r
mayorMenores (Nodo _ _ hder) = mayorMenores hder