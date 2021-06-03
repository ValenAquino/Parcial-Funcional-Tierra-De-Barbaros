module Library where
import PdePreludat
import Data.Char (toUpper, isUpper)

-- point free: cuando nos ahorramos de pedirle uno o mas parametros porque son a quienes aplicarle directamente una función establecida
-- esMultiploDe a b = ((==0).( `mod` a)) b  ===>  la b se podría evitar, sin modificar la función   ===>   esMultiploDe a = (==0).( `mod` a)

data Barbarian = Barbarian{
    name :: String,
    strength :: Number,
    skills :: [Skill],
    objects :: [Object]
} deriving Show

instance Eq Barbarian where
  (Barbarian name strength skills objects) == (Barbarian name' strength' skills' objects') =
     name == name'  && strength == strength' && skills == skills && length objects == length objects'

type Object = Barbarian -> Barbarian
type Skill = String
type Aventura = Barbarian -> Bool
type Evento = Barbarian -> Bool

-- Punto 1

espada :: Number -> Object
espada swordLarge barbarian = barbarian {strength = strength barbarian + 2 * swordLarge}

amuletoMisticoPuercoMarrano :: Skill -> Object
amuletoMisticoPuercoMarrano skill barbarian = barbarian {skills = skills barbarian ++ [skill]}

varitasDefectuosas :: Object
varitasDefectuosas barbarian = barbarian {skills = skills barbarian ++ ["Hacer Magia"], objects = []}

ardilla :: Object
ardilla = id

cuerda :: Object -> Object -> Object
cuerda = (.)


-- Punto 2

megafono :: Object
megafono barbarian = barbarian {skills = map ponerEnMayuscula (skills barbarian) }

ponerEnMayuscula :: String -> String
ponerEnMayuscula = map toUpper

megafonoMegaBarbarico :: Object
megafonoMegaBarbarico = cuerda ardilla megafono


-- Punto 3

condicionDeSupervivencia :: [Evento] -> Barbarian -> Bool
condicionDeSupervivencia condiciones barbaro = any ((== True) . (\condicion -> condicion barbaro)) condiciones


invasionDeSuciosDuendes :: Aventura
invasionDeSuciosDuendes = condicionDeSupervivencia [escribePoesiaAtroz]

escribePoesiaAtroz :: Evento
escribePoesiaAtroz barbaros =  "Escribir Poesía Atroz" `elem` skills barbaros

cremalleraDelTiempo :: Aventura
cremalleraDelTiempo = condicionDeSupervivencia [tenerPulgares]

tenerPulgares :: Evento
tenerPulgares barbarian = name barbarian == "Faffy" || name barbarian == "Astro"

ritualDeFechorias :: Aventura
ritualDeFechorias = condicionDeSupervivencia [saqueo, gritoDeGuerra, caligrafia]

saqueo :: Evento
saqueo barbaro = "Robar" `elem` skills barbaro || strength barbaro > 80

gritoDeGuerra :: Evento
gritoDeGuerra barbaro =  poderDeGritoDeGuerra barbaro == cantidadDeLetrasDeTodasSusHabilidades barbaro

poderDeGritoDeGuerra :: Barbarian -> Number
poderDeGritoDeGuerra = (4 *).length.objects

cantidadDeLetrasDeTodasSusHabilidades :: Barbarian -> Number
cantidadDeLetrasDeTodasSusHabilidades = sum . map length . skills

caligrafia :: Evento
caligrafia barbaro =  all tieneTresVocalesYempiezaConMayuscula (skills barbaro)

tieneTresVocalesYempiezaConMayuscula :: String ->  Bool
tieneTresVocalesYempiezaConMayuscula habilidad = tieneTresVocales habilidad && empiezaConMayuscula habilidad

tieneTresVocales :: String -> Bool
tieneTresVocales = (== 3) . length . filter (`elem` lasVocales)

empiezaConMayuscula :: String ->  Bool
empiezaConMayuscula = isUpper.head

lasVocales :: [Char]
lasVocales = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']

sobrevivientes :: Aventura -> [Barbarian] -> [Barbarian]
sobrevivientes = filter

-- Punto 4

sinRepetidosRecursiva :: Barbarian -> Barbarian
sinRepetidosRecursiva barbarian = barbarian { skills = descartarRepetidos (skills barbarian) }

descartarRepetidos :: [Skill] -> [Skill]
descartarRepetidos [] = []
descartarRepetidos (habilidad : habilidades)
 | habilidad `elem` habilidades = descartarRepetidos habilidades
 | otherwise = habilidad : descartarRepetidos habilidades

sinRepetidos :: [String] -> [String]
sinRepetidos = foldl (\habilidades habilidad -> habilidades ++ agregarSiNoEsta habilidad habilidades ) []

agregarSiNoEsta :: String -> [String] -> [String]
agregarSiNoEsta habilidad habilidades
 | habilidad `elem` habilidades = []
 | otherwise = [habilidad]

--sinRepetidos :: [Skill] -> [Skill]
--sinRepetidos = filter (\)

-- >>> sinRepetidos ["Hola", "Hola", "Chau", "Chau", "Martes", "Hola", "Chau","Hola", "Chau","Hola", "Chau","Hola", "Chau", "Jueves"]
-- ["Hola","Chau","Martes","Jueves"]
