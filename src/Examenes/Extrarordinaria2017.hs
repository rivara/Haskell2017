
module Examenes.Extrarordinaria2017 where
-- EJERCICIO2 A

separacion::[Int]->[[Int]]
separacion xs =[foldr(\a b -> if (estaRepetido a xs) then a:b else b)[] xs ,foldr(\a b -> if (not (estaRepetido a xs)) then a:b else b)[] xs]

estaRepetido::Int->[Int]->Bool
estaRepetido x xs = if (length(foldr(\a b -> if (a==x) then a:b else b)[] xs)>1) then True else False


-- B
cuentaPitagoricas :: [(Int,Int,Int)] -> Int
cuentaPitagoricas ((a,b,c):xs) = length[(a,b,c)|(a,b,c) <- xs, a*a+b*b==c*c]
-- EJERCICIO3


data UniLinea= U{timestamp::String , fichero::String ,funcion::String, mensage::String}
data MultiLinea= M{timestamp'::String , fichero'::String ,funcion'::String, mensage'::[String]}

data Log = L{uni::[UniLinea] , multiline::[MultiLinea]}

instance Eq Log where
	(==)(L u1 m1) (L u2 m2)= u1==u2 && m1==m2
	

--class Structurual a where 
-- 	compara:: a->a->Bool
 
 --instance Structurual log where
 --	compara