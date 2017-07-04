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

-- hermanos (Rama (Rama AVacio 1 AVacio) 3 (Rama AVacio 2 AVacio)) 1 2




--  VERSION 1
--type Jugador = String
--data Set = S (Int, Int) -- type Set = (Int, Int)
--data Partido = P [Set] -- type Partido = [Set]
--data Torneo = TO {nombre :: String, finalista1 :: Jugador, finalista2 :: Jugador, partido :: Partido}
--
--
--instance Eq Torneo where
--  (==) t1 t2 = (nombre t1) == (nombre t2)
--
--instance Ord Torneo where
--  (<) t1 t2 = (nombre t1) < (nombre t2)
--  (<=) t1 t2 = t1 < t2 || (nombre t1) == (nombre t2)
--
--instance Show Torneo where
--  show t = (nombre t) ++ ", Ganador: " ++ g ++ ", en " ++ show (sets t) ++ " sets."
--    where
--      g = ganador t
--
--sets :: Torneo -> Int
--sets (TO _ _ _ (P ss)) = length ss
--
--ganador :: Torneo -> Jugador
--ganador = ganadorAux (0, 0)
--
--ganadorAux :: (Int, Int) -> Torneo -> Jugador
--ganadorAux (s1, s2) (TO _ f1 f2 (P [])) = if s1 > s2 then f1 else f2
--ganadorAux (s1, s2) (TO n f1 f2 (P ((S (j1, j2)):ss))) = ganadorAux (if j1 > j2 then (s1 + 1, s2) else (s1, s2 + 1)) (TO n f1 f2 (P ss))
--
--instance Show Temporada' where
--  show (TE []) = ""
--  show (TE (t:ts)) = show t ++ "\n" ++ show (TE ts)
--
--temporada2013 :: Temporada'
-- temporada2013 = TE [TO "Open de Australia" "Novak Djokovic" "Andy Murray" (P [S (6, 7), S (7, 6), S (6, 3), S (6, 2)]), TO "Indian Wells" "Juan Martín del Potro" "Rafael Nadal" (P [S (6, 4), S (3, 6), S (4, 6)]), TO "Mutua Madrid Open" "Rafel Nadal" "Stanislas Wawrinka" (P [S (6, 2), S (6, 4)]), TO "Wimbledon" "Novak Djokovic" "Andy Murray" (P [S (6, 4), S (7, 5), S (6, 4)])]
--
--qs :: Ord a => [a] -> [a]
--qs [] = []
--qs (p:xs) = qs [x | x <- xs, x < p] ++ [p] ++ qs [x | x <- xs, x >= p]
--
--mostrarListadoOrdenadoTorneos :: Temporada' -> String
--mostrarListadoOrdenadoTorneos (TE ts) = show (TE (qs ts))


--
---- Representa los datos de uno de los finalistas, con su lista de puntos para cada set
---- cuando más longitud tenga la lista, más puntos han jugado
--data Finalista = Final Nombre [Int] deriving Show
--data Finalistas = Finalist Finalista Finalista deriving Show
--data Torneo = Tor Nombre Finalistas
--data Temporada = Temp [Torneo] deriving Show
--
-- --  VERSION 2
--instance Show Torneo where
--show torneo = getNombre (torneo) ++ ", Ganador: "++ getNombreFinalista (ganador torneo) ++ ", en " ++ show (numeroSets (torneo)) ++ " sets." ++ "\n"
--
--instance Eq Torneo where
--(Tor n1 f1) == (Tor n2 f2) = n1 == n2
--
--instance Ord Torneo where
--(Tor n1 f1) <= (Tor n2 f2) = n1 <= n2 
--(Tor n1 f1) > (Tor n2 f2) = n1 > n2 
--(Tor n1 f1) >= (Tor n2 f2) = n1 >= n2 
--(Tor n1 f1) < (Tor n2 f2) = n1 < n2
--
--
--getNombre:: Torneo -> String 
--getNombre (Tor n _) = n
--
--getNombreFinalista :: Finalista -> String getNombreFinalista (Final n lista) = n
--
--numeroSets :: Torneo -> Int
--numeroSets (Tor n finalistas) = numeroSetsTorneo finalistas
--
--numeroSetsTorneo :: Finalistas -> Int
--numeroSetsTorneo (Finalist (Final n1 lista1) (Final n2 lista2)) = length lista1
--
--ganador :: Torneo -> Finalista
--ganador (Tor _ finalistas) = ganador' finalistas
--
--ganador' :: Finalistas -> Finalista
--ganador' (Finalist f1 f2) = ganadorAux f1 f2 0 0
--
---- Como es información del mismo torneo, se supone que ambas listas son del
---- mismo tamaño
----ganadorAux :: Finalista -> Finalista -> Int -> Int -> Finalista
--ganadorAux (Final n1 []) (Final n2 l) cont1 cont2 = if (cont1 > cont2) then (Final n1 []) else (Final n2 [])
--ganadorAux (Final n1 (x:xs)) (Final n2 (y:ys)) cont1 cont2 = if x > y then
--ganadorAux (Final n1 xs) (Final n2 ys) (cont1+1)cont2 else ganadorAux (Final n1 xs) (Final n2 ys) cont1 (cont2+1)
--mostrarListadoOrdenadoTorneos :: Temporada -> String mostrarListadoOrdenadoTorneos (Temp []) = "" mostrarListadoOrdenadoTorneos (Temp torneos) = mostrar (qs torneos)
--
--mostrar :: [Torneo] -> String mostrar [] = ""
--mostrar (x:xs) = show x ++ mostrar xs
--
--
---- DECLARACIÓN DE DATOS --
--
--openAustralia :: Torneo
--openAustralia = Tor "Open de Australia" (Finalist (Final "Novak Djokovic" [6,7,6,6]) (Final "Andy Murray" [7,6,3,2]))
--
--indianWells :: Torneo
--indianWells = Tor "Indian Wells" (Finalist (Final "Juan Martin del Potro" [6,3,4]) (Final "Rafael Nadal" [4,6,6]))
--
--mutuaMadridOpen :: Torneo
--mutuaMadridOpen = Tor "Mutua Madrid Open" (Finalist (Final "Rafael Nadal" [6,6]) (Final "Stanislas Wawrinka" [2,4]))
--
--wimbledon :: Torneo
--wimbledon = Tor "Wimbledon" (Finalist (Final "Andy Murray" [6,7,6]) (Final "Novak Djokovic" [4,5,4]))
--
--temporada2013 :: Temporada
--temporada2013 = Temp [openAustralia, indianWells, mutuaMadridOpen, wimbledon]