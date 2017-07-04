
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



-- VERSION1

data Version = V {major :: Int, minor :: Int}
data Library = L {name :: String, version :: Version}

instance Show Version where
	show (V ma mi) = show ma ++ "." ++ show mi

instance Eq Version where
	(==) (V ma1 mi1) (V ma2 mi2) = ma1 == ma2 && mi1 == mi2
	(/=) v1 v2 = not (v1 == v2)
	
instance Ord Version where
	(<) (V ma1 mi1) (V ma2 mi2) = (ma1 < ma2) || ((ma1 == ma2) && (mi1 < mi2))
	(<=) v1 v2 = (v1 < v2) || (v1 == v2)
	(>) (V ma1 mi1) (V ma2 mi2) = (ma1 > ma2) || ((ma1 == ma2) && (mi1 > mi2))
	(>=) v1 v2 = (v1 > v2) || (v1 == v2) 

instance Show Library where
	show (L n v) = show n ++ ": " ++ show v

instance Eq Library where
	(==) (L n1 v1) (L n2 v2) = n1 == n2 && v1 == v2
	(/=) l1 l2 = not (l1 == l2)
	
instance Ord Library where
	(<) (L n1 v1) (L n2 v2) = (n1 < n2) || ((n1 == n2) && (v1 < v2))
	(<=) l1 l2 = (l1 < l2) || (l1 == l2)
	(>) (L n1 v1) (L n2 v2) = (n1 > n2) || ((n1 == n2) && (v1 > v2))
	(>=) l1 l2 = (l1 > l2) || (l1 == l2)

class Compatible a where
	areCompatibles :: a -> a -> Bool

instance Compatible Library where
	areCompatibles (L n1 v1) (L n2 v2) = (n1 == n2) && ((major v1) == (major v2))

compatibleLibraries :: [Library] -> Library -> [Library]
compatibleLibraries ls lin = foldl (\lr l -> if areCompatibles lin l then lr ++ [l] else lr) [] ls

--VERSION2

type Name= String
data Version'  =Ve{major'::Int,minor'::Int} deriving Show
data Library' =Li{name'::String,version'::Version',dependencies::[Library]} deriving Show

class Compatible' a where
	compatible'::a->a->Bool

instance Eq Version' where
	v1==v2 = major' v1 == major' v1 && minor' v1== minor' v2

instance Ord Version' where
    v1 < v2 = major' v1 < major' v2 || major' v1 == major' v2 && minor' v1 < minor' v2
    v1 <= v2 = major' v1 < major' v2 || major' v1 == major' v2 && minor' v1 <= minor' v2
    v1 > v2 = major' v1 > major' v2 || major' v1 == major' v2 && minor' v1 > minor' v2
    v1 >= v2 = major' v1 > major' v2 || major' v1 == major' v2 && minor' v1 >= minor' v2

instance Eq Library' where
    l1 == l2 = name' l1 == name' l2 && version' l1 == version' l2

instance Ord Library' where
    l1 > l2 = name' l1 == name' l2 && version' l1 > version' l2
    l1 < l2 = name' l1 == name' l2 && version' l1 < version' l2
    l1 <= l2 = name' l1 == name' l2 && version' l1 <= version' l2
    l1 >= l2 = name' l1 == name' l2 && version' l1 >= version' l2

instance Compatible' Library' where
   compatible' l1 l2 = name' l1 /= name' l2 || major' (version' l1) == major' (version' l2)

checkCompatibility :: Library' -> [Library'] -> [Library']
checkCompatibility l ls = [x | x <- ls, compatible' x l]

