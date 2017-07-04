  
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
igualesAux r (x:xs)= igualesAux (r++[buscar x xs])(elimina x xs) 

buscar::(Eq a)=>a->[a]->[a]
buscar x ys =foldr(\a b->if(a==x)then a:b else b)[x] ys

elimina::(Eq a)=>a->[a]->[a]
elimina x ys =foldr(\a b->if(a==x)then b else a:b)[] ys


-- Iva
type Precio = Float

data Musica = Musica Precio
instance Show Musica where
  show (Musica p) = "Musica (" ++ show p ++ " Euros)"

data Libro = Libro Precio
instance Show Libro where
  show (Libro p) = "Libro (" ++ show p ++ " Euros)"

class Producto p where
  precioBase :: p -> Float
  ivaImponible :: p -> Float
  tasaAplicable :: p -> Float

instance Producto Musica where
  precioBase (Musica p) = p
  ivaImponible _ = 21
  tasaAplicable (Musica p) = p + (p * ((ivaImponible (Musica p)) / 100))

instance Producto Libro where
  precioBase (Libro p) = p
  ivaImponible _ = 15
  tasaAplicable (Libro p) = p + (p * ((ivaImponible (Libro p)) / 100))

pvp :: Producto p => p -> Float
pvp = tasaAplicable 
