
module Examenes.Extraordinaria2016 where
-- E2

-- ej "holaAmigo" --> ("oaAio","hlmg")

ej :: String -> (String, String)
ej = foldl (\ (vs, cs) c -> if (esConsonante (c)) then (vs ++ [c], cs) else (vs, cs ++ [c])) ([], [])

esConsonante :: Char->Bool
esConsonante x= case x of
				'a'-> True
				'e'-> True
				'i'-> True
				'o'-> True
				'u'-> True
				'A'-> True
				'E'-> True
				'I'-> True
				'O'-> True
				'U'-> True	
				otherwise ->False

-- E3
--class Joinable v where{
-- 	union::v a->v a->v a
--} 

-- ?????????

--class Joinable2 ([]) where{
-- 	union:: l1 l2= l1++l2
--	} 


data Arbol a = Vacio | Rama (Arbol a) a (Arbol a) deriving Show 
-- ?????????

--instance Joinable Arbol where{
-- 	 union Vacio Vacio = Vacio
--	 union Vacio ar = ar
 -- 	 union ar Vacio = ar
--  	 union (Rama hizq1 r1 hder1) ar2 = (Rama (union hizq1 ar2) r1 hder1)
--	}
--E4
foldl'::[Int]->[[Int]]
foldl'= foldAux[]

foldAux::[[Int]]->[Int]->[[Int]]
foldAux r []=reverse(r)
foldAux r (x:xs)= foldAux(r++[[sum(x:xs)]]) xs
