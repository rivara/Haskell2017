
module Examenes.Oct2016 where
import Data.Char
--E2
-- examen cambiar en posiciones pares
-- examen[1,2,3,4,4,2] 4 3
-- impares

sustituir::(Eq a)=>[a]->a->a->[a]
sustituir xs y z=foldr(\a b -> if(even(length b)) then if(a==y)then z:b else a:b else a:b)[] xs
-- sustituir [1,5,2,7,1,4,9,-2,-6,1,4,8,11] 1 0 == [0,5,2,7,0,4,9,-2,-6,1,4,8,11]

--JOSE
sustituir' :: (Eq a) => [a] -> a -> a -> [a]
sustituir' lista x y = foldr (\e lista2 -> 
							if ((e == x) && (posImpar lista lista2)) then  y:lista2 else e:lista2) [] lista

posImpar :: (Eq a) => [a] -> [a] -> Bool
posImpar l1 l2 = odd ((length l1) - (length l2))









-- E3
-- cambio ["Madrid","Barcelona"]     .> ["mADRID","bARCELONA"]
cambio::[String]->[String]
cambio xs =[cambioAux x|x<-xs]

cambioAux::String->String
cambioAux xs = [toLower x|x <- xs , isUpper x]++[toUpper x|x<-xs,isLower x]



-- JOSE

cambio' :: [String] -> [String]
cambio' listaCadena = map (cambioAux') listaCadena
--cambio listaCadena = foldl (\e listaSol -> (cambioAux e):listaSol) [] listaCadena

cambioAux' :: String -> String
cambioAux' elemento = foldl (\cadenaSol c -> if isUpper c then cadenaSol++[toLower c] else cadenaSol++[toUpper c]) [] elemento
