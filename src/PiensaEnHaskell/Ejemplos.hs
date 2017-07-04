
module PiensaEnHaskell.Ejemplos where

replicate' :: Int -> a -> [a]
replicate' =  replicateAux []

replicateAux::[a]->Int -> a -> [a]
replicateAux r 0 _= r
replicateAux r n x = replicateAux (x:r) (n-1) x


-- map' con foldl
map'::[a]->(a->a)->[a]
map' xs f= foldl(\a b -> (f b):a)[] xs
 
 
-- filter' con foldr

--filter'::[a]->(f->Bool)->[a]
--filter' xs f= foldr(\a b -> if(f a)then b else b)[] xs


-- Ejercicio Inventado
--Se quiere informatizar una biblioteca  usando para ello haskell
--la biblioteca  , tendra un tipo que sera Libros y cada libro se compondrá de una signatura un titulo y un autor.
--A continuación existirá otro tipo Catalago que contendra los libros y el genero de estos
--
--ejemplo:
-- libro (“1”,”Dracula,”Bram Stoker”)
-- libro (“2”,”It,”Stephen king”)
-- genero ([“1”,”Dracula,”Bram Stoker”,“2”,”It,”Stephen king”],terror)
--
--se pide crear  ordenar libros y una clase Catalogos la cual contara cuantos géneros hay en la biblioteca , 
--se llamara a esta clase por medio de la función	 totalGeneros

data Libro= L{signatura::String,titulo::String,autor::String}
data Catalogo= C{libros::[Libro],genero::String}

-- INSTANCIAS SHOW EQ ORD de LIBRO

instance Eq Libro where
	(==)(L s1 t1 a1)(L s2 t2 a2)= s1==s2 || t1==t2 && a1==a2
	

instance Show Libro where
	show(L s t a)= "Libro"++show t ++"signatura"++ show s ++"Autor"++show a
	
	
instance Ord Libro where
	(<)(L s1 t1 a1)(L s2 t2 a2)= s1<s2 
	(>)(L s1 t1 a1)(L s2 t2 a2)= s1>s2 
	(<=)(L s1 t1 a1)(L s2 t2 a2)=s1<=s2 
	(>=)(L s1 t1 a1)(L s2 t2 a2)=s1>=s2 
	


-- INSTANCIAS SHOW EQ ORD DE CATALOGO

instance Eq Catalogo where
	(==)(C l1 g1)(C l2 g2)= l1==l2 && g1==g2 

instance Show Catalogo where
	show(C l g)= "Libroa"++show l ++"genero"++ show g 
	
	
	
	
class Generos p where
	cuenta::p ->Int

instance Generos Catalogo where
	cuenta(C l g)= length l	


cuentaGeneros::Catalogo->Int
cuentaGeneros c = cuenta c

--cuentaGeneros (C [L "1" "Dracula" "Bram Stoker",L "2" "It" "Stephen king"] "terror")


-- PRUEBAS
llenaLibro::(String,String,String)->Libro
llenaLibro(s,t,a) = L s t a
--libr("2","It","Stephen king")
 
llenaLibro'::Libro->Libro
llenaLibro'(L s t a) = L s t a

--libr'(L "2" "It" "Stephen king")
llenaCatalogo::[Libro]->String->Catalogo
llenaCatalogo l genero=(C l genero)

--llenaCatalogo [L "1" "Dracula" "Bram Stoker",L "2" "It" "Stephen king"] "terror" 
--llenaCatalogo [L "1" "Dracula" "Bram Stoker",L "2" "It" "Stephen king"] "terror" 

