
module PiensaEnHaskell.Ejemplos where

replicate' :: Int -> a -> [a]
replicate' =  replicateAux []

replicateAux::[a]->Int -> a -> [a]
replicateAux r 0 _= r
replicateAux r n x = replicateAux (x:r) (n-1) x

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
	show (L n v) = n ++ ": " ++ show v

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



map'  :: (a->b) -> [a] -> [b]
map' f  [] =  []
map' f (x:xs) =  f x : map' f xs
-- map' (*2) [1,2,3]