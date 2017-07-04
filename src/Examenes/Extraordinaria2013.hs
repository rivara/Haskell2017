--tarde
module Examenes.Extraordinaria2013 where
--2) Se pide implementar un programa en Haskell que sea capaz de obtener el número de errores de una tabla de multiplicar, junto con los elementos de la tabla con los errores. Por ejemplo, si recibe la siguiente tabla del 1 el resultado debería de ser algo como:
--  "Hay 0 errores, que son: ".
--1 × 0 = 0
--1 × 1 = 1
--1 × 2 = 2
--1 × 3 = 3
--1 × 4 = 4
--1 × 5 = 5
--1 × 6 = 6
--1 × 7 = 7
--1 × 8 = 8
--1 × 9 = 9
--1 × 10 = 10
--Sin embargo si recibe la siguiente tabla del 3 el resultado debería de ser algo como:

--Hay 4 errores, que son: 4x1=3, 3x3=10, 3x6=20, 3x3=24".
--3 × 0 = 0 4 × 1 = 3
--3 × 2 = 6 3 × 3 = 10
--3 × 4 = 12
--3 × 5 = 15 3 × 6 = 20
--3 × 7 = 21 3 × 3 = 24
--3 × 9 = 27
--3 × 10 = 30
-- FilaTabla 
data FilaTabla= FT{operando1::Int, operando2::Int,resultado::Int}
data TablaMultiplicar= TM[FilaTabla]

instance Eq FilaTabla where
	(==)(FT o11 o12 r1)(FT o21 o22 r2)= o11==o21 && o12==o22 && r1==r2
	
	
instance Show FilaTabla where
	show (FT o1 o2 r)= show o1 ++"*"++show o2 ++"="++show r
	
instance Show TablaMultiplicar where
	show (TM[])="\n"
	show (TM(ft:fts))=show ft ++"\n"++ show (TM fts)
	
--tablaDeMultiplicar
tablaDeMultiplicar::Int->TablaMultiplicar
tablaDeMultiplicar op1=TM[FT op1 op2 (op2 * op1)|op2<-[1..10]]

p::Int->Int->Int->FilaTabla
p  op1 op2 op3= (FT op1 op2 op3) 
 
--errores
errores:: TablaMultiplicar->String
errores (TM[]) ="La tabla de multiplicar esta vacia"
errores (TM(ft:fts)) = "Hay"++ show (length fts) ++"errores,son" ++ show fts

erroresAux::[FilaTabla]-> TablaMultiplicar -> TablaMultiplicar ->[FilaTabla]
erroresAux r (TM[])(TM[])=r
erroresAux r (TM(fto:ftos))(TM(ftg:ftgs))= erroresAux (if (fto==ftg) then r else r++[fto]) (TM ftos) (TM ftgs)

-- errores[TM [FT 1 2 3 ,FT 1 2 3]]


--4) Se pide implementar un programa en Haskell que sea capaz de obtener las estadísticas de los
--	 resultados de un equipo de fútbol en una temporada determinada. 
--Debe ser lo suficientemente general como para que se pueda aplicar a diferentes temporadas y en
--  diferentes ligas.
--Una temporada se compone de diferentes jornadas, cada jornada se compone de una serie de encuentros 
-- y cada encuentro contiene información de los dos equipos que y del resultado final del partido, 
--como se puede ver en la Figura 1.
--Figura 1. Ejemplo de las dos primeras jornadas de la liga de la temporada 2010/2011
--Un ejemplo podría ser una liga con 4 equipos (R. Madrid, Valencia, Betis y Atlético de Madrid), donde los resultados de las 3 únicas jornadas de la temporada pasada fueron:
--Jornada 1:
--R. Madrid – Betis (2-1)
--Atlético de Madrid – Valencia (6,1)
--Jornada 2:
--Valencia – R. Madrid (1,5)
--Betis – Atlético de Madrid (3,3)
--Jornada 3:
--Betis –Valencia (0,0)
--R. Madrid – Atlético de Madrid (3,3)
-- Si se quieren conocer las estadísticas del R. Madrid para dicha temporada un ejemplo de aplicación de función podría ser el siguiente:--
-- > estadisticas(madrid,temporada2010_2011)
--(Ganados: 2, Empatados: 1, Perdidos: 0)
--Se valorará positivamente la claridad y extensibilidad del código, así como la definición de tipos de datos adecuados para la resolución del problema.

data Marcador =Int :-:Int
data ResultadoEncuentro=Ganado|Empatado|Perdido deriving Eq
data Encuentro = E{equipoLocal::String,equipoVistante::String,marcador::Marcador}
data Jornada=J[Encuentro] 
data Temporada= T[Jornada]
data Estadisticas=ES{ganados::Int,empatados::Int,perdidos::Int}


instance Show Estadisticas where
	show (ES g e p) = "(Ganados: " ++ show g ++ ", Empatados: " ++ show e ++ ", Perdidos: " ++ show p ++ ")" 

jornada1 :: Jornada
jornada1 = J [E "Madrid" "Betis" (2 :-: 1), E "Atletico de Madrid" "Valencia" (6 :-: 1)]

jornada2 :: Jornada
jornada2 = J [E "Valencia" "RMadrid" (1 :-: 5), E "Betis" "Atletico de Madrid" (3 :-: 3)]

jornada3 :: Jornada
jornada3 = J [E "Betis" "Valencia" (0 :-: 0), E "R.Madrid" "Atletico de Madrid" (3 :-: 3)]

temporada2010_2011 :: Temporada
temporada2010_2011 = T[jornada1, jornada2, jornada3]

estadisticas::String->Temporada->Estadisticas
estadisticas=estadisticasAux (ES 0 0 0)

estadisticasAux::Estadisticas->String->Temporada->Estadisticas
estadisticasAux r _(T[])=r
estadisticasAux (ES g e p) ne (T(j:js))=estadisticasAux (if re == Ganado then (ES (g + 1) e p) else if re == Empatado then (ES g (e + 1) p) else (ES g e (p + 1))) ne (T js)
  where
    ed = encuentroDisputado ne j
    re = resultadoEncuentro ne ed
    
    
contieneEquipo :: Encuentro -> String -> Bool
contieneEquipo (E el ev _) ne = ne == el || ne == ev

encuentroDisputado :: String -> Jornada -> Encuentro -- Damos por hecho que el equipo va a estar presente
encuentroDisputado ne (J (e:es)) = if (contieneEquipo e ne) then e else encuentroDisputado ne (J es)

resultadoEncuentro :: String -> Encuentro -> ResultadoEncuentro
resultadoEncuentro ne (E el ev (gl :-: gv)) = if ne == el then if gl > gv then Ganado else if gl == gv then Empatado else Perdido else if gv > gl then Ganado else if gv == gl then Empatado else Perdido



