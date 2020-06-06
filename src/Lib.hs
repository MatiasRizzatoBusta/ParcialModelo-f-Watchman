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
type Vigilante = (String,[String],Int)

destruccionDeNiuShork :: [Vigilante]->[Vigilante]
destruccionDeNiuShork = filter (not.sacoVigilante "Rorschach").filter (not.sacoVigilante "Dr.manhattan")

sacoVigilante :: String->Vigilante->Bool
sacoVigilante vigilanteASacar (nombre,habilidades,anio) = nombre == vigilanteASacar

