
module Examenes.Oct2014 where

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
insertarMesaLibre r(Ocupacion [] mo)m= Ocupacion(r++[m]) mo
insertarMesaLibreAux r (Ocupacion (ml:mls) mo) m = if (capacidad m) <= (capacidad ml) then 
							Ocupacion (r ++ (m:ml:mls)) mo else 
							insertarMesaLibreAux (r ++ [ml]) (Ocupacion mls mo) m

							
							


ocuparMesa :: Ocupacion -> Int -> Ocupacion
ocuparMesa = ocuparMesaAux []

ocuparMesaAux :: Mesas -> Ocupacion -> Int -> Ocupacion
ocuparMesaAux r (Ocupacion [] mo) _ = Ocupacion r mo
ocuparMesaAux r (Ocupacion (ml:mls) mo) n = if (capacidad ml) >= n then  Ocupacion (r ++ mls) (mo ++ [ml]) else ocuparMesaAux (r ++ [ml]) (Ocupacion mls mo) n
							