
module Examenes.Octubre2013 where

data Arbol a= AVacio |Rama (Arbol a) a (Arbol a) deriving (Show)

hermanos:: (Eq a) => Arbol a -> a -> a -> Bool
hermanos AV _ _ = False
hermanos (Rama AV r AV) _ _ = False
hermanos (Rama AV r (Rama deri rd derd)) h1 h2 =
		hermanos (Rama deri rd derd) h1 h2
			hermanos (Rama (Rama izi ri izd) r AV) h1 h2 == hermanos (Rama izi ri izd) h1 h2
					hermanos (Rama (Rama izi ri izd) r (Rama deri rd derd)) h1 h2
						  |((ri==h1) && (rd==h2)) || ((ri==h2) && (rd==h1)) = True
						|otherwise = (hermanos (Rama izi ri izd) h1 h2 ||hermanos (Rama deri rd derd) h1 h2)