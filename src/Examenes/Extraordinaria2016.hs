
module Examenes.Extraordinaria2016 where
-- E2

eje2::String->(String,String)
eje2 xs= foldr(\a b ->(([a]++b),([a]++b)))( , )xs
--eje2 xs= foldr(\a b -> if(esConsonante a)then (b,b++[a]) else (b++[a],b)) (,) xs

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
-- class Joinable a where{
-- 	union:: a->a->[a]
-- } 

-- class Joinable ([]) where{
-- 	union:: [a]->[a]->[a]
-- } 


--E4
foldl'::[Int]->[[Int]]
foldl'= foldAux[]

foldAux::[[Int]]->[Int]->[[Int]]
foldAux r []=reverse(r)
foldAux r (x:xs)= foldAux(r++[[sum(x:xs)]]) xs
