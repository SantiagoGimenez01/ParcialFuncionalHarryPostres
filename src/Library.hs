module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

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
 