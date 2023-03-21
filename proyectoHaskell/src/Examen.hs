module Examen where

import Data.List
import Data.Char

--Ejercicio 1

data Arbol a = AV | Rama (Arbol a) a (Arbol a)

--alturaArbol :: Arbol -> Int
--alturaArbol 

--Ejercicio 2
type Arte = String

type Nombre = String

type Nacimiento = Int    

data Artista = A Nombre Arte Nacimiento

artistas :: [Artista]
artistas = [A "Cervantes" "Literatura" 1547,

            A "Velazquez" "Pintura" 1599,

            A "Picasso" "Pintura" 1881,

            A "Beethoven" "Musica" 1770,

            A "Poincare" "Ciencia" 1854,

            A "Quevedo" "Literatura" 1580,

            A "Goya" "Pintura" 1746,

            A "Einstein" "Ciencia" 1879,

            A "Mozart" "Musica" 1756,

            A "Botticelli" "Pintura" 1445,

            A "Borromini" "Arquitectura" 1599,

            A "Bach" "Musica" 1685]

instance Show Artista where
    show(A nom art naci) = (nom ++ " - " ++ art ++ ", nacido en " ++ show(naci))
 
pintores :: [Artista] -> [Artista]
pintores lista = [(A n art naci) | (A n art naci) <- lista, art == "Pintura" && naci > 1500]


--Ejercicio 3
--Apartado A
data TipoEvento = EventoFamiliar | Boda | FiestaInfantil | ReunionEmpresa deriving (Eq,Show)

data TipoDeporte = Escalada | Saltos | Bolos deriving (Eq,Show) 

data TipoEspacio = EspInfantil | EspDeporte | EspLoft deriving Eq

data Espacio = Esp {tipo :: TipoEspacio, nombre :: String, direccion :: String, capacidad :: Int, cocina :: Bool, ocio :: TipoDeporte}

data Empresa = Emp {nom :: String, espacios :: [Espacio]} deriving Eq

--Apartado B
esPosible :: Espacio -> TipoEvento -> Bool
esPosible (Esp tipo _ _ _ _ _) t
    | (tipo == EspInfantil) && (t == FiestaInfantil) = True
    | (tipo == EspDeporte) && (t == ReunionEmpresa || t == FiestaInfantil) = True
    | (tipo == EspLoft) && (t == ReunionEmpresa || t == Boda || t == EventoFamiliar) = True
    | otherwise = False

--Apartado C
espaciosPorEventos :: Empresa -> TipoEvento -> [Espacio]
espaciosPorEventos (Emp _ esp) tipo = foldr (\x acu -> if esPosible x tipo then x : acu else acu) [] esp

--Apartado D

instance Eq Espacio where
    (Esp t1 _ _ c1 _ _) == (Esp t2 _ _ c2 _ _) = if t1 == t2 && c1 == c2 then True else False

espaciosIguales :: (Eq Espacio) => Empresa -> Empresa -> [Espacio]
espaciosIguales (Emp _ esps1) (Emp _ esps2) = [y | y <- esps1, y `elem` esps2]

