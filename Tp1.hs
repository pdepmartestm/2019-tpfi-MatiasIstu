--estructuras
--type Criterio = (Pirata->Tesoro->Pirata)
type Tesoro = (String,Int)
data Pirata = Pirata {nombre::String,botin::[Tesoro]} deriving(Show)
data Barco = Barco {tripulacion::[Pirata]}deriving(Show)
--1
--maneraDeSaquear::Criterio--

cantidadDeTesoros :: Pirata -> Int
cantidadDeTesoros  = length.botin

esAfortunado :: Pirata->Bool
esAfortunado pirata= (>10000).sum.(map snd) $ (botin pirata)
--obtenerNombres pirata = map snd (botin pirata)

tienenMismoObjetoConDiferenteValor :: Pirata->Pirata->Bool
tienenMismoObjetoConDiferenteValor pirata1 pirata2 = (>1).length.(filter(igualNombreDistintoValor)).(zip (botin pirata1).botin) $ pirata2

igualNombreDistintoValor (tupla1,tupla2) = (fst tupla1) ==(fst tupla2) && snd tupla1 /= (snd tupla2)

--[((nombre1,precio1),(nombre2,precio2))]

elTesoroMasValioso:: Pirata->Int
elTesoroMasValioso pirata = maximum (map snd (botin pirata))

adquirirTesoro :: Pirata -> Tesoro -> Pirata
adquirirTesoro pirata tesoro = pirata {botin = botin pirata ++ [tesoro]}

perderTodosLosTesorosValiosos :: Pirata->Pirata
perderTodosLosTesorosValiosos pirata = pirata {botin = filter(not.esValioso) (botin pirata)}

esValioso :: Tesoro->Bool
esValioso tesoro = (>100).snd $ tesoro

perderTodosLosTesorosSegunNombre :: Pirata-> String -> Pirata
perderTodosLosTesorosSegunNombre pirata nombre = pirata {botin = filter(tieneDeNombre nombre) (botin pirata)}

tieneDeNombre :: String -> Tesoro ->Bool
tieneDeNombre nombre tesoro = (esIgualA nombre).fst $ tesoro

esIgualA :: String ->String->Bool
esIgualA nombre1 nombre2 = nombre1==nombre2

--Modelos

jackSparrow = Pirata "jackSparrow" [("brujula",10000),("frascoDeArena",0)]
davidJones = Pirata "davidJones" [("cajaMusical",1)]
anneBonny = Pirata "anneBonny" [("doblones",100),("frascoDeArena",1)]

--2

--Formas de saqueo

--soloTesorosValiosos pirata tesoro
-- | esValioso(tesoro) = adquirirTesoro pirata tesoro
-- | otherwise = id

--tesorosConNombreEspecifico pirata tesoro
-- | tieneDeNombre (nombreTesoro pirata) tesoro = adquirirTesoro pirata tesoro
-- | otherwise = id

tieneCorazon pirata tesoro = id

--cumpleAlguna pirata tesoro
-- | soloTesorosValiosos || tesorosConNombreEspecifico = adquirirTesoro pirata tesoros
-- |otherwise = id

--Saquear
saquear formaDeSaqueo tesoro pirata = formaDeSaqueo pirata tesoro

--3
seLlama pirata1 pirata2 = (nombre pirata1) /= (nombre pirata2)

incorporarPirata :: Barco->Pirata ->Barco
incorporarPirata barco pirata = barco {tripulacion = tripulacion barco ++ [pirata]}

sacarPirata :: Barco->Pirata ->Barco
sacarPirata barco pirata = barco {tripulacion = filter(noSellama pirata) (tripulacion barco)}

anclarEnIsla barco tesoro = map (adquirirTesoro tesoro) (tripulacion barco)

--Ataque

--atacarUnaCiudad barco tesoros
-- | length tesoros > length (tripulacion barco) = agregarTesorosAPiratas barco tesoros
-- | otherwise = (tirarPiratas tesoros).(agregarTesorosAPiratas barco) tesoros

agregarTesorosAPiratas barco tesoros = zipWith (saqueo barco) (tripulacion barco) tesoros

saqueo barco tupla = saquear(formaDeSaqueo barco) (fst tupla) (snd tupla)

tirarPiratas barco tesoros = take (length tesoros) (barco tripulacion)

abordarOtroBarco barco = barco {tripulacion = []}
