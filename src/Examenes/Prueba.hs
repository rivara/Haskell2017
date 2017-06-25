
module Examenes.Prueba where


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

data Colection a = C[a] 

class Tab a where{
	tabular::[a]->[a]
}

--instance Tab Colection where{
--	tabular c=(C a)++"c"
--}









