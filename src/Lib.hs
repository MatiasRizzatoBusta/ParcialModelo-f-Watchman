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
type Habilidades = [String]
type Vigilante = (String,Habilidades,Int)--como es de mas de dos elementos no puedo hacer fst y snd
type Agente = (String,String)
type Evento = [Vigilante]->[Vigilante]

tomoElPrimero :: Vigilante->String
tomoElPrimero (nombre,_,_) = nombre--como es de mas de dos elementos tengo que acceder a los datos asi

destruccionDeNiuShork :: Evento
destruccionDeNiuShork = filter (not.sacoVigilante "Rorschach").filter (not.sacoVigilante "Dr.manhattan")

sacoVigilante :: String->Vigilante->Bool
sacoVigilante vigilanteASacar (nombre,_,_) = (== vigilanteASacar) nombre

muerteDeUnVigilante :: String->Evento
muerteDeUnVigilante nombre = filter (sacoVigilante nombre)

guerraDeVietnam :: Evento
guerraDeVietnam  = map (agregarHabilidad "cinismo").esAgenteDelGobierno 

agregarHabilidad :: String->Vigilante->Vigilante
agregarHabilidad habilidad (nombre, habilidades, anio) = (nombre,habilidad:habilidades,anio)
-- no le hago ++ pq eso es solo para concatenar 2 listas. : lo agrega siempre al comienzo
 
esAgenteDelGobierno :: [Vigilante]->[Vigilante]
esAgenteDelGobierno vigilantes = filter (estaEn agentesDelGobierno) vigilantes
--le paso la lista con los nombres de los agentes del gob. y me fijo si el vigilante que agarra el filter esta en esa lista.
--en caso de que este lo guarda en la lista nueva con solo los agentes del gob y les agrego a todos cinismo en habilidades.

estaEn :: [Agente]->Vigilante->Bool
estaEn agentes (nombre,_,_)= any (==nombre) (map fst agentes)

accidenteDeLab :: Int->[Vigilante]
accidenteDeLab anio = ("Dr.Manhattam",["manipulacion de materia a nivel atomico"],anio):algunosVigilantes
--de vuelta uso : y no ++ pq solo le agrego un elemento y no una lista de elementos

actaDeKeene :: [Vigilante]->[Vigilante]
actaDeKeene listaEntera |any (comparoNombres listaEntera) algunosVigilantes = sacoVigilanteAnterior listaEntera (head listaEntera)
                        |otherwise = listaEntera -- si no hay sucesor devielve la lista como esta

comparoNombres :: [Vigilante]->Vigilante->Bool
comparoNombres lista (nombre,_,_) = ((==nombre).tomoElPrimero.head) lista

sacoVigilanteAnterior :: [Vigilante]->Vigilante->[Vigilante]
sacoVigilanteAnterior lista vigilante = dropWhile (esSucesor vigilante) lista 

esSucesor:: Vigilante->Vigilante->Bool
esSucesor (nombre,_,anio) (otroNombre,_,otroAnio) = (nombre == otroNombre) && (anio > otroAnio)