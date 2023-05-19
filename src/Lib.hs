{-
Tierra de Bárbaros
Rechapos! - En la tierra de Udrogoth los reyes bárbaros formaron un imperio de guerreros, dragones y magia.
Ahora marcharon a la guerra contra el mal y dejaron a sus herederos a cargo.... y bueno, hay que conformarse.
Sin embargo, nos encargaron confeccionar un programa en Haskell para manejar los asuntos del reino, y evitar
así el tener que gobernar.
Consideraciones:
Escribir el tipo de todas las funciones principales
Emplear sinónimos de tipo cuando sea posible.
No se permite usar recursividad salvo que se indique lo contrario
Definir las funciones en estilo point-free cuando sea posible
Punto 1
Se sabe que los bárbaros tienen nombre, fuerza, habilidades y objetos,
que los ayudarán más adelante en su lucha contra el mal. Por ejemplo:
-}

data Barbaro = Barbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
}

type Objeto = Barbaro -> Barbaro
type Habilidad = String


{-
Se pide definir los siguientes objetos y definir algunos bárbaros de ejemplo
* Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.
* Los amuletosMisticos otorgan una habilidad dada a un bárbaro.
* Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.
* Una ardilla, que no hace nada.
* Una cuerda, que combina dos objetos distintos, obteniendo uno que realiza las transformaciones de los otros dos.
-}

espadas :: Int -> Objeto
espadas unPeso =  mapFuerza (+ (unPeso * 2))

mapFuerza :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerza unaFuncion unBarbaro = unBarbaro { fuerza = unaFuncion (fuerza unBarbaro) }

amuletosMisticos :: Habilidad -> Objeto
amuletosMisticos unaHabilidad = agregarHabilidad unaHabilidad

agregarHabilidad :: Habilidad -> Barbaro -> Barbaro
agregarHabilidad unaHabilidad unBarbaro = mapHabilidades (unaHabilidad :) unBarbaro

mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Barbaro -> Barbaro
mapHabilidades unaFuncion unBarbaro = unBarbaro { habilidades = unaFuncion (habilidades unBarbaro) }

varitasDefectuosas :: Objeto
varitasDefectuosas = mapObjetos (const []) . agregarHabilidad "hacerMagia"

mapObjetos :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetos unaFuncion unBarbaro = unBarbaro { objetos = unaFuncion (objetos unBarbaro) }

ardilla :: Objeto
ardilla unBarbaro = unBarbaro

cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto1 objeto2 = objeto2 . objeto1

{-
Punto 2
El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas.
*Main> megafono dave
Barbaro "Dave" 100 ["TEJERESCRIBIRPOESIA"] [<function>,<function>]
Sabiendo esto, definir al megafono, y al objeto megafonoBarbarico, que está formado por una cuerda, una ardilla y un megáfono.
-}

megafono :: Objeto
megafono unBarbaro = mapHabilidades (\habilidades -> [map toUpper (concat habilidades)]) unBarbaro

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono

{-
Punto 3 - Aventuras
Los bárbaros suelen ir de aventuras por el reino luchando contra las fuerzas del mal,
pero ahora que tienen nuestra ayuda, quieren que se les diga si un grupo de bárbaros puede
sobrevivir a cierta aventura. Una aventura se compone de uno o más eventos, por ejemplo:
* invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”
* cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen 
pulgares, los demás sí.
* ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes:
* saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
* gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades.
El poder necesario para aprobar es 4 veces la cantidad de objetos del bárbaro.
* caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si sus habilidades 
contienen más de 3 vocales y comienzan con mayúscula.
Sabiendo esto, se pide:
Definir los eventos, modelar las aventuras y dar un ejemplo.
-}

type Aventura = [Evento]
type Evento = Barbaro -> Bool

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes unBarbaro = sabe "Escribir Poesía Atroz" unBarbaro

sabe :: Habilidad -> Barbaro -> Bool
sabe unaHabilidad unBarbaro = elem unaHabilidad (habilidades unBarbaro)

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = (not . tienePulgares . nombre) unBarbaro

tienePulgares :: String -> Bool
tienePulgares "Faffy" = False
tienePulgares "Astro" = False
tienePulgares _       = True

type Prueba = Evento

ritualDeFechorias :: [Prueba] -> Evento
ritualDeFechorias unaPrueba unBarbaro = any ( $ unBarbaro) unaPrueba

-- saqueo
-- saqueo

-- gritoDeGuerra
-- gritoDeGuerra

-- caligrafia
-- caligrafia