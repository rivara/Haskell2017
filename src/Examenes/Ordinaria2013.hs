
module Examenes.Ordinaria2013 where

-- criba eratostenes


erastotenes :: Int -> [Int]
erastotenes n = erastotenesAux [] [2..n]

erastotenesAux :: [Int] -> [Int] -> [Int]
erastotenesAux r [] = r
erastotenesAux r (e:es) = erastotenesAux (r ++ [e]) (cribar e es)

cribar :: Int -> [Int] -> [Int]
cribar = cribarAux []

cribarAux :: [Int] -> Int -> [Int] -> [Int]
cribarAux r _ [] = r
cribarAux r n (e:es) = cribarAux (if e `rem` n == 0 then r else r ++ [e]) n es


-- hermanos REVISAR

data Arbol a= AVacio |Rama (Arbol a) a (Arbol a) deriving (Show)

hermanos :: Eq a => Arbol a -> a -> a -> Bool
hermanos AVacio _ _ = False
hermanos (Rama AVacio r AVacio) h1 h2 = False
hermanos (Rama AVacio r hder) h1 h2 = hermanos hder h1 h2
hermanos (Rama hizq r AVacio) h1 h2 = hermanos hizq h1 h2
hermanos (Rama (Rama hizq1 r1 hder1) r (Rama hizq2 r2 hder2)) h1 h2 = (r1 == h1 && r2 == h2 || r1 == h2 && r2 == h1) || hermanos (Rama hizq1 r1 hder1) h1 h2 || hermanos (Rama hizq2 r2 hder2) h1 h2 
