module Lib where
import Text.Show.Functions
laVerdad = True

-------------Vigilantes-------------
algunosVigilantes = [ ("El Comediante", ["Fuerza"], 1942),
 ("Buho Nocturno", ["Lucha", "Ingenierismo"], 1963), 
 ("Rorschach", ["Perseverancia", "Deduccion", "Sigilo"], 1964),
 ("Espectro de Seda", ["Lucha", "Sigilo", "Fuerza"], 1962),
 ("Ozimandias", ["Inteligencia", "Más Inteligencia Aún"], 1968), 
 ("Buho Nocturno", ["Lucha", "Inteligencia", "Fuerza"], 1939), 
 ("Espectro de Seda", ["Lucha", "Sigilo"], 1940)]

-------------Agentes del Gob-------------
agentesDelGobierno = [("Jack Bauer","24"),
 ("El Comediante", "Watchmen"), 
 ("Dr. Manhattan", "Watchmen"), 
 ("Liam Neeson", "Taken")]


--------Eventos------------
type Vigilante = (String,[String],Int)--como es de mas de dos elementos no puedo hacer fst y snd
type Agente = (String,String)
type Evento = [Vigilante]->[Vigilante]

tomoElPrimero :: Vigilante->String
tomoElPrimero (nombre,_,_) = nombre

destruccionDeNiuShork :: Evento
destruccionDeNiuShork = filter (not.sacoVigilante "Rorschach").filter (not.sacoVigilante "Dr.manhattan")

sacoVigilante :: String->Vigilante->Bool
sacoVigilante vigilanteASacar (nombre,_,_) =  nombre == vigilanteASacar

muerteDeUnVigilante :: String->Evento
muerteDeUnVigilante nombre = filter (sacoVigilante nombre)

guerraDeVietnam :: Evento
guerraDeVietnam  = map (agregarHabilidad "cinismo").esAgenteDelGobierno 

agregarHabilidad :: String->Vigilante->Vigilante
agregarHabilidad habilidad (nombre, habilidades, anio) = (nombre,habilidad:habilidades,anio)
-- no le hago ++ pq eso es solo para concatenar 2 listas. : lo agrega siempre al comienzo
 
esAgenteDelGobierno :: [Vigilante]->[Vigilante]
esAgenteDelGobierno vigilantes = filter (estaEn agentesDelGobierno) vigilantes

estaEn :: [Agente]->Vigilante->Bool
estaEn agentes (nombre,_,_)= any (==nombre) (map fst agentes)