module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate, AsyncException (ThreadKilled))

-- Personas de ejemplo

dave :: Barbarian 
dave = Barbarian "Dave" 100 ["tejer","escribirPoesia"] [ardilla, espada 5]



correrTests :: IO ()
correrTests = hspec $ do
 
 describe "Punto 1" $ do
   describe "espada" $ do
      it "una espada que pesa 5 kilos le da a Dave 10 de fuerza extra" $ do
         espada 5 dave `shouldBe` dave {strength = 110}
   describe "amuletos" $ do
      it "dave puede ser un ingeniero magico ahora que tiene su amuleto mistico puerco marrano" $ do
         amuletoMisticoPuercoMarrano "Ingeniero Magico" dave `shouldBe` dave {skills = ["Ingeniero Magico","tejer","escribirPoesia"]}
   describe "varitas defectuosas" $ do
      it "Gracias a su varita defectuosa, dave ahora tiene la habilidad de Hacer magia" $ do
         varitasDefectuosas dave `shouldBe`  dave { skills = ["tejer", "escribirPoesia", "Hacer Magia"], objects = []}
   describe "ardillas" $ do
      it "de objeto que nada se esperaba ... NADA ESTÁ HACIENDO" $ do
         ardilla dave `shouldBe`  dave
   describe "cuerda" $ do
      it "Gracias a la cuerda, Dave ahora puede ser un ingeniero magico y hacer magia a la vez" $ do
         cuerda varitasDefectuosas (amuletoMisticoPuercoMarrano "Ingeniero Magico") dave `shouldBe` dave { skills = ["tejer","escribirPoesia", "Ingeniero Magico", "Hacer Magia"], objects = []}

 describe "Punto 2" $ do
    describe "megafono" $ do
      it "dave con su megafono POTENCIA TODAS SUS HABILIDADES" $ do
         megafono dave `shouldBe` dave{ skills = ["TEJER","ESCRIBIRPOESIA"]}
    describe "megafonoMegaBarbarico" $ do
      it "dave con un megafono megabarbarico POTENCIA TODAS SUS HABILIDADES pero no mas que eso" $ do
         megafonoMegaBarbarico dave `shouldBe` dave{ skills = ["TEJER","ESCRIBIRPOESIA"]}

 describe "Punto 3" $ do
    describe "Invasión de Duendes" $ do
      it "Dave sobrevive a la invasion de duendes porque sabe escribir poesia atroz" $ do
         invasionDeSuciosDuendes dave{skills = ["Escribir Poesía Atroz"]} `shouldBe` True 
      it "Dave no sobrevive a la invasion de duendes porque no sabe escribir poesia atroz" $ do
         invasionDeSuciosDuendes dave `shouldBe` False 
    describe "Cremallera Del Tiempo" $ do
      it "Dave no sobrevive porque tiene pulgares" $ do
         cremalleraDelTiempo dave `shouldBe`  False 
      it "Faffy y Astro sobreviven porque no tienen pulgares" $ do
         cremalleraDelTiempo dave {name = "Faffy"} `shouldBe` True 
         cremalleraDelTiempo dave {name = "Astro"} `shouldBe` True
    describe "Ritual de Fechorias" $ do
      it "Dave el barbaro ladrón sobrevive al ritual desde que empezó a pulir su habilidad de robar" $ do
         ritualDeFechorias dave{skills = ["Robar"]} `shouldBe` True
      it "Dave el barbaro gritón sobrevive al ritual" $ do
         ritualDeFechorias dave{skills = ["tejer", "ver"]} `shouldBe` True
      it "Dave el barbaro correcto sobrevive al ritual desde que mejoró su ortografía" $ do
         ritualDeFechorias dave{skills = ["Cocinar", "Estudiar"]} `shouldBe` True
    describe "Sobrevivientes" $ do
      it "El unico sobreviviente a cremallera del tiempo es Fuffy" $ do
         sobrevivientes cremalleraDelTiempo  [dave, dave{name = "Faffy"}] `shouldBe`  [dave{name="Faffy"}]

 describe "Punto 4" $ do
    describe "" $ do
      it "" $ do
         2+2 `shouldBe`  4

 describe "Punto 4" $ do
    describe "" $ do
      it "" $ do
         2+2 `shouldBe`  4

escribime :: Expectation
escribime = implementame
