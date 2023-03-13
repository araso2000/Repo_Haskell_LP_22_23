module T5L3 where

import Data.List
import Data.Char

--Tema 5 - Listado 3 - Ejercicio 1
data Fecha = Fecha { dia :: Int, mes :: Int, year :: Int }
instance Show Fecha where 
    show (Fecha d m a) = show d ++ "/" ++ show m ++ "/" ++ show a

--Tema 5 - Listado 3 - Ejercicio 2
--Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

--Tema 5 - Listado 3 - Ejercicio 3
divisiones :: (Integral a, Show a) => a -> [a] -> [String]
divisiones _ [] = []
divisiones n (x:xs) = if x == 0 then "Nothing" : divisiones n xs else ("Just " ++ show (n `div` x)) : divisiones n xs

--Tema 5 - Listado 3 - Ejercicio 4
data Titulacion = GradoII | GradoII_ADE | GradoADE deriving (Show,Eq)

data Estudiante = Estudiante { nombre :: String, titulacion :: Titulacion } deriving (Show, Eq)

type ListaEstudiantes = [Estudiante]

type ListaAsociaciones = [Estudiante]

mostrarAlumnosAsociaciones :: ListaEstudiantes -> ListaAsociaciones -> String
mostrarAlumnosAsociaciones matriculados asociaciones =
    let asociados = filter (\e -> e `elem` asociaciones) matriculados
    in concatMap (\e -> "(" ++ nombre e ++ "," ++ show (titulacion e) ++ ")") asociados


matriculados = [ Estudiante "Carlos Calle" GradoII_ADE
               , Estudiante "Irene Plaza" GradoADE
               , Estudiante "Juan Lopez" GradoII
               , Estudiante "Maria Perez" GradoII_ADE
               ]

asociaciones = [ Estudiante "Carlos Calle" GradoII_ADE
               , Estudiante "Irene Plaza" GradoADE
               ]

--Tema 5 - Listado 3 - Ejercicio 5
data Fecha' = Fecha' {dia' :: Int, mes' :: Int, ano' :: Int}

instance Show Fecha' where
  show(Fecha' d1 m1 a1) = show(d1) ++ "/" ++ show(m1) ++ "/" ++ show(a1)

--Apartado A
instance Eq Fecha' where
  (Fecha' d1 m1 a1) == (Fecha' d2 m2 a2) = d1==d2 && m1==m2 && a1==a2


mismaFecha :: (Eq Fecha') => Fecha' -> Fecha' -> Bool
mismaFecha f1 f2 = f1 == f2

--Apartado B
instance Ord Fecha' where
  (Fecha' d1 m1 a1) <= (Fecha' d2 m2 a2) =
    a1 < a2 || (a1 == a2 && (m1 < m2 || (m1 == m2 && d1 <= d2)))

ordenar :: (Ord Fecha') => [Fecha'] -> [Fecha']
ordenar = sort

ordenarYMostrar :: [Fecha'] -> String
ordenarYMostrar = ("[" ++) . (++ "]") . intercalate "," . map mostrarFecha
  where mostrarFecha (Fecha' d m a) = show d ++ "/" ++ show m ++ "/" ++ show a

--Apartado C
--Pide usar quicksort, pero como ya está programado más arriba, no es necesario más código que el de prueba

--Tema 5 - Listado 3 - Ejercicio 6
data Pila a = Pil [a] deriving Show
data Cola a = Col [a] deriving Show

class Coleccion c where
  esVacia :: c a -> Bool
  insertar :: a -> c a -> c a
  primero :: c a -> a
  eliminar :: c a -> c a
  size :: c a -> Int

instance Coleccion Pila where
  esVacia (Pil xs) = null xs
  insertar x (Pil xs) = Pil (x:xs)
  primero (Pil xs) = head xs
  eliminar (Pil xs) = Pil (tail xs)
  size (Pil xs) = length xs

instance Coleccion Cola where
  esVacia (Col xs) = null xs
  insertar x (Col xs) = Col (xs ++ [x])
  primero (Col xs) = head xs
  eliminar (Col xs) = Col (tail xs)
  size (Col xs) = length xs


--Tema 5 - Listado 3 - Ejercicio 7
data Torneo = Torneo { nombreTorneo :: String, ganador :: String, sets :: Int} deriving Eq

data Temporada = Temporada { nombreTempo :: String, torneos :: [Torneo]}

instance Show Torneo where
  show(Torneo n g s) = n ++ ", Ganador: " ++ g ++ ", en " ++  show(s) ++ " sets."

instance Show Temporada where
  show (Temporada n []) = ""
  show(Temporada n (t:ts)) = show(t) ++ "\n" ++ show(Temporada n ts)

instance Ord Torneo where
  (Torneo n1 g1 s1) <= (Torneo n2 g2 s2) = (head n1) <= (head n2)

mostrarListadoOrdenadoTorneos :: (Ord Torneo) => Temporada -> String
mostrarListadoOrdenadoTorneos temp = show(temp)

--let temporada2013 = Temporada "temporada2013" [(Torneo "Indian Wells" "Rafael Nadal" 3),(Torneo "Mutua Madrid Open" "Rafael Nadal" 2),(Torneo "Open de Australia" "Novak Djokovic" 4),(Torneo "Wimbledon" "Andy Murray" 3)]

--Tema 5 - Listado 3 - Ejercicio 8
--insert :: (Ord a) => Tree a -> a -> Tree a
--insert Empty x = leaf x
--insert (Node x l r) y = case compare y x of
--          GT -> Node x l (insert r y)
--          _ -> Node x (insert l y) r

data Student = Student {fullname:: String, age::Integer, qualifications:: [Integer]}