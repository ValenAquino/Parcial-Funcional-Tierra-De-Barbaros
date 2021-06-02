module Library where
import PdePreludat
import Text.Show.Functions
import Data.Char (toUpper, isUpper)

-- point free: cuando nos ahorramos de pedirle uno o mas parametros porque son a quienes aplicarle directamente una función establecida
-- esMultiploDe a b = ((==0).( `mod` a)) b  ===>  la b se podría evitar, sin modificar la función   ===>   esMultiploDe a = (==0).( `mod` a)

