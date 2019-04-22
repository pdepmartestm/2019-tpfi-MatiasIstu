--estructuras
type Criterio = Pirata->Tesoro->Pirata
type Tesoro = (String,Int)
data Pirata = Pirata {nombre::String,botin::[Tesoro]} deriving(Show)
data Barco = Barco {formaDeSaqueo::Criterio,tripulacion::[Pirata]}
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
soloTesorosValiosos :: Pirata->Tesoro->Pirata
soloTesorosValiosos pirata  tesoro  | esValioso(tesoro) = adquirirTesoro pirata tesoro | otherwise = pirata

--Saquear
saquear :: (Pirata->Tesoro->Pirata) ->Tesoro->Pirata->Pirata
saquear formaDeSaqueo tesoro pirata = formaDeSaqueo pirata tesoro

--3
seLlama pirata1 pirata2 = (nombre pirata1) /= (nombre pirata2)

incorporarPirata :: Barco->Pirata ->Barco
incorporarPirata barco pirata = barco {tripulacion = tripulacion barco ++ [pirata]}

sacarPirata :: Barco->Pirata ->Barco
sacarPirata barco pirata = barco {tripulacion = filter(not.seLlama pirata) (tripulacion barco)}

anclarEnIsla barco tesoro = map (flip adquirirTesoro tesoro) (tripulacion barco)

--Ataque

atacarUnaCiudad barco tesoros | length tesoros > length (tripulacion barco) = agregarTesorosAPiratas barco tesoros
                              | otherwise = take (length tesoros).(agregarTesorosAPiratas barco) $ tesoros

agregarTesorosAPiratas :: Barco->[Tesoro]->[Pirata]
agregarTesorosAPiratas barco tesoros = map (saqueo barco) (zip (tripulacion barco) tesoros)


--[(pirata,tesoro),(pirata,tesoro),(pirata,tesoro)]

saqueo :: Barco->(Pirata,Tesoro)-> Pirata
saqueo barco tupla = saquear(formaDeSaqueo barco) (snd tupla) (fst tupla)

tirarPiratas piratas tesoros = take (length tesoros) piratas

abordarOtroBarco barco = barco {tripulacion = []}
