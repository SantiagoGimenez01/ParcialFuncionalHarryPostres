module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--PARTE 1
data Postre = UnPostre{
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
}deriving(Show, Eq)

type Sabor = String

type Hechizo = Postre -> Postre

tarta :: Postre
tarta = UnPostre{sabores = ["melaza"], peso = 50, temperatura = 0}

bizcochoBorracho :: Postre
bizcochoBorracho = UnPostre{sabores = ["fruta", "crema"], peso = 100, temperatura = 25}

volcan :: Postre
volcan = UnPostre{sabores = ["chocolate"], peso = 200, temperatura = 20}

incendio :: Hechizo
incendio = perderPeso 5 . calentarPostre 1

calentarPostre :: Number -> Postre -> Postre
calentarPostre grados postre = postre{temperatura = temperatura postre + grados}

perderPeso :: Number -> Postre -> Postre
perderPeso porcentaje postre = postre{peso = peso postre * (1 - (porcentaje / 100))}

immobulus :: Hechizo
immobulus = congelarPostre

congelarPostre :: Postre -> Postre
congelarPostre postre = postre{temperatura = 0}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = perderPeso 10 . levantarYDejarCaer

levantarYDejarCaer :: Postre -> Postre
levantarYDejarCaer = agregarSabor "concentrado"

diffindo :: Number -> Hechizo
diffindo = perderPeso

riddikulus :: Sabor -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

agregarSabor :: Sabor -> Postre -> Postre
agregarSabor sabor postre = postre{sabores = sabor:sabores postre}

avadaKedabra :: Hechizo
avadaKedabra = perderSabores . immobulus

perderSabores :: Postre -> Postre
perderSabores postre = postre {sabores = []} 

mesaDePostresListos :: [Postre] -> Hechizo -> Bool
mesaDePostresListos postres hechizo = all (postreListo . hechizo) postres

postreListo :: Postre -> Bool
postreListo postre = temperatura postre > 0 && peso postre > 0 && (not . null . sabores) postre

pesoPromedioDePostresListos :: [Postre] -> Number
pesoPromedioDePostresListos postres = (sum . pesosPostres . filtrarPostresListos) postres / cantidadPostresListos postres

filtrarPostresListos :: [Postre] -> [Postre]
filtrarPostresListos = filter postreListo

cantidadPostresListos :: [Postre] -> Number
cantidadPostresListos = length . filtrarPostresListos

pesosPostres :: [Postre] -> [Number]
pesosPostres = map peso 

--PARTE 2
data Mago = UnMago{
    hechizosAprendidos :: [Hechizo],
    cantidadHorrocruxes :: Number
}deriving(Show, Eq)

harry :: Mago
harry = UnMago{hechizosAprendidos=[incendio, immobulus, levantarYDejarCaer], cantidadHorrocruxes = 0}

asistirAClases :: Mago -> Hechizo -> Postre -> Mago
asistirAClases mago hechizo postre
    | hechizo postre == avadaKedabra postre = (sumarHorrocrux . practicarConHechizo hechizo) mago
    | otherwise = practicarConHechizo hechizo mago

practicarConHechizo :: Hechizo -> Mago -> Mago
practicarConHechizo hechizo mago = mago{hechizosAprendidos = hechizo:hechizosAprendidos mago}

mismoResultadoQueAvKe :: Hechizo -> Postre -> Bool
mismoResultadoQueAvKe hechizo postre = hechizo postre == avadaKedabra postre

sumarHorrocrux :: Mago -> Mago
sumarHorrocrux mago = mago{cantidadHorrocruxes = cantidadHorrocruxes mago + 1}

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldr1 (cualHechizoEsMejor postre) (hechizosAprendidos mago)

cualHechizoEsMejor :: Postre -> Hechizo -> Hechizo -> Hechizo
cualHechizoEsMejor postre hechizo1 hechizo2
    | cantidadDeSaboresPostHechizo postre hechizo1 > cantidadDeSaboresPostHechizo postre hechizo2 = hechizo1
    | otherwise = hechizo2

cantidadDeSaboresPostHechizo :: Postre -> Hechizo -> Number
cantidadDeSaboresPostHechizo postre hechizo = (length . sabores . hechizo) postre

--Parte 3

postresInfinitos :: [Postre] -> [Postre]
postresInfinitos = cycle

ron :: Mago
ron = UnMago{hechizosAprendidos = cycle [incendio, wingardiumLeviosa, immobulus], cantidadHorrocruxes = 2}

{-
    b) Si existe una consulta y es: MesaDePostresListos postresInfinitos avadaKedabra
    Esto retorna false ya que al encontrar un postre que no esta listo la funcion devuelve el resultado (false), esto lo hace
    debido a que haskell trabaja con evaluacion diferida y cuando tiene que controlar que TODOS los postres esten listos
    basta con encontrar uno solo que no lo este para que ahi termine el control y devuelva que al menos uno no lo esta por lo que
    es FALSO que todos estan listos.

    c)No existe tal caso ya que para poder encontrar el mejor hechizo haskell necesitaria evaluar cada uno de los hechizos y 
    compararlos de modo que nunca terminaria el control debido a que la lista de hechizos es infinita. En este caso a diferencia
    del anterior si requiere que pase por cada hechizo ya que no basta con comparar hasta cierto punto ya que debido a la 
    infinitud de la lista siempre puede llegar a haber un hechizo mejor y por lo tanto siempre seguiria buscando y comparando
    con los infinitos hechizos que se encuentran en la lista.

-}