
module Examenes.Ordinaria2015 where
-- E2 a
--  filter' odd [1,2,3,4] ->[1,3] 
filter'::(a->Bool)->[a]->[a]
filter' f xs = reverse(foldl(\a b->if(f b)then (b:a) else a) [] xs) 

-- b mmap [\n->tail n, \n ->map]["Hola","Adios"]    .>[]
mmap::[(a->a)]->[a]->[a]
mmap fs xs= foldr(\a b -> [aux fs a ]++b)[] xs

aux ::[(a->a)]->a->a
aux [] r= r
aux (f:fs) r= aux fs (f r)

-- aux[(*2),(*3)] 1
-- aux[tail,head] "hola"


-- E3
-- iguales [1,2,2,2,4,4,7,8] [[1][2,2,2][4,4][8]]  -- repetios
iguales::(Eq a)=>[a]->[[a]]
iguales=igualesAux[[]]

igualesAux::(Eq a)=>[[a]]->[a]->[[a]]
igualesAux r []=r
igualesAux r (x:xs)= igualesAux r++[n|n<- xs ,x==n] xs


-- Iva

