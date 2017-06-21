
module Examenes.Ordinaria2014 where
--- pertenece  y elimima

pertenece::(Eq a)=> a->[a]->Bool
pertenece _ [a]=True
pertenece a [] = False
pertenece x ys= if(x`elem`(foldr(\a b -> a:b)[] ys))then True else False

eliminar ::(Eq a)=>a->[a]->[a]
eliminar _ [a] =[a]
eliminar a[]=[]
eliminar x ys = foldr(\a b -> if(a==x)then b else a:b)[] ys


--Mesa (Revsar)

data Mesa=M{numero::Int,capacidad::Int}
data Ocupacion= O{libres::[Mesa],ocupadas::[Mesa]} 

--mesa
instance Eq Mesa where
	(==)(M m1 c1)(M m2 c2)= c1==c2
	
instance Ord Mesa where 
	(>=)(M m1 c1)(M m2 c2)= c1>=c2
	(<=)(M m1 c1)(M m2 c2)= c1<=c2
	(<)(M m1 c1)(M m2 c2)= c1<c2
	(>)(M m1 c1)(M m2 c2)= c1>c2

instance Show Mesa where
 show(M m c)= "la mesa "++ show m ++" tiene "++ show c++" sillas de ocupacion"

instance Show Ocupacion where
 show(O l o)= "libres "++ show l ++" Ocupado "++ show o 


insertarMesaLibre::Ocupacion->Mesa->Ocupacion
insertarMesaLibre r(O [] mo)m=O(r++[m]) mo
insertarMesaLibreAux r (O (ml:mls) mo) m = if (capacidad m) <= (capacidad ml) then 
							O (r ++ (m:ml:mls)) mo else 
							insertarMesaLibreAux (r ++ [ml]) (O mls mo) m

							
							


ocuparMesa :: Ocupacion -> Int -> Ocupacion
ocuparMesa = ocuparMesaAux []

ocuparMesaAux :: [Mesa] -> Ocupacion -> Int -> Ocupacion
ocuparMesaAux r (O [] mo) _ = O r mo
ocuparMesaAux r (O (ml:mls) mo) n = if (capacidad ml) >= n then  O (r ++ mls) (mo ++ [ml]) else ocuparMesaAux (r ++ [ml]) (O mls mo) n
							