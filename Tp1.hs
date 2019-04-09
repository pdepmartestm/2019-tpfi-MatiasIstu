--estructuras

type Tesoro = (String,Int)
data Pirata = Pirata {nombre::String,botin::[Tesoro],nombreTesoro::String} deriving(Show)
data Barco = Barco {maneraDeSaquear::Criterio,tripulacion::[Pirata]}deriving(Show)
--1

cantidadDeTesoros :: Pirata -> Int
cantidadDeTesoros  = lenght.botin

esAfortunado :: Pirata->Bool
esAfortunado pirata = (>10000).sum.map snd (botin pirata)

--tienenMismoObjetoConDiferenteValor :: Pirata->Pirata->Bool
--tienenMismoObjetoConDiferenteValor pirata1 pirata2 =


elTesoroMasValioso:: Pirata->Int
elTesoroMasValioso pirata = maximum (map snd (botin pirata))

adquirirTesoro :: Pirata -> Tesoro -> Pirata
adquirirTesoro pirata tesoro = pirata {botin = botin pirata ++ [tesoro]}

perderTodosLosTesorosValiosos :: Pirata->Pirata
perderTodosLosTesorosValiosos pirata = pirata {botin = filter(not.esValioso) (botin pirata)}

esValioso :: Tesoro->Bool
esValioso tesoro = (>100).snd tesoro

perderTodosLosTesorosSegunNombre :: Pirata-> String -> Pirata
perderTodosLosTesorosSegunNombre pirata nombre = pirata {botin = filter(tieneDeNombre nombre) (botin pirata)}

tieneDeNombre :: String -> Tesoro ->Bool
tieneDeNombre nombre tesoro = (esIgualA nombre).snd tesoro

esIgualA :: String ->String->Bool
esIgualA nombre1 nombre2 = nombre1==nombre2

--Modelos

jackSparrow = pirata jackSparrow [(brujula,10000),(frascoDeArena,0)]
davidJones = pirata davidJones [(cajaMusical,1)]
anneBonny = pirata anneBonny [(doblones,100),(frascoDeArena,1)]

--2

--Formas de saqueo

soloTesorosValiosos pirata tesoro | esValioso(tesoro) = adquirirTesoro pirata tesoro
 | otherwise = id

tesorosConNombreEspecifico pirata tesoro | tieneDeNombre (nombreTesoro pirata) tesoro = adquirirTesoro pirata tesoro
| otherwise = id

tieneCorazon pirata tesoro = id

cumpleAlguna | soloTesorosValiosos || tesorosConNombreEspecifico = adquirirTesoro pirata tesoros
| otherwise = id

--Saquear
saquear formaDeSaqueo tesoro pirata = formaDeSaqueo pirata tesoro

--3

incorporarPirata :: Barco->Pirata ->Barco
incorporarPirata barco pirata = barco {tripulacion = tripulacion barco ++ [pirata]}

sacarPirata :: Barco->Pirata ->Barco
sacarPirata barco pirata = barco {tripulacion = filter(not.tieneDeNombre pirata) (tripulacion barco)}

anclarEnIsla barco tesoro = map (adquirirTesoro tesoro) (tripulacion barco)

--Ataque

atacarUnaCiudad barco tesoros | lenght tesoros > lenght (tripulacion barco) = agregarTesorosAPiratas barco tesoros
| otherwise = (tirarPiratas tesoros).(agregarTesorosAPiratas barco) tesoros

agregarTesorosAPiratas barco tesoros = zipWith (saqueo barco) (tripulacion barco) tesoros

saqueo barco tupla = saquear(formaDeSaqueo barco) (fst tupla) (snd tupla)

tirarPiratas barco tesoros = take (lenght tesoros) (barco tripulacion)

abordarOtroBarco barco = barco {tripulacion = []}
