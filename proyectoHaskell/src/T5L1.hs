module T5L1 where

import Data.List
import Data.Char

--Tema 5 - Listado 1 - Ejercicio 1
type Nombre = String
type Edad = Integer
type Persona = (Nombre, Edad) 

descuentoAbono :: Persona -> Bool
descuentoAbono (_, e) = if e < 26 || e >= 65 then True else False

--Tema 5 - Listado 1 - Ejercicio 2
type Expediente = Integer
type Dni = String
type NotaPD = Float

type Alumno = (Dni, Expediente, NotaPD)

--Apartado A:
aprobado :: Alumno -> Bool
aprobado (_, _, n) = if n >= 5 then True else False

--Apartado B:
calificacionAlumno :: Alumno -> String
calificacionAlumno (_, e, n) = "Expediente: " ++ show e ++ " - Nota acta: " ++ show n

--Apartado C:
dameNota :: Alumno -> NotaPD
dameNota (_, _, n) = n

--Apartado D:
mediaNotas :: [Alumno] -> Float
mediaNotas [] = 0
mediaNotas (x:xs) = (dameNota x + mediaNotas xs) / 2


--Tema 5 - Listado 1 - Ejercicio 3
data Alumno' = Alum Dni Expediente NotaPD deriving Show

--Apartado A:
aprobado' :: Alumno' -> Bool
aprobado' (Alum _ _ n) = if n >= 5 then True else False

--Apartado B:
calificacionAlumno' :: Alumno' -> String
calificacionAlumno' (Alum _ e n) = "Expediente: " ++ show e ++ " - Nota acta: " ++ show n

--Apartado C:
dameNota' :: Alumno' -> NotaPD
dameNota' (Alum _ _ n) = n

--Apartado D:
mediaNotas' :: [Alumno'] -> Float
mediaNotas' [] = 0
mediaNotas' (x:xs) = (dameNota' x + mediaNotas' xs) / 2

--Tema 5 - Listado 1 - Ejercicio 4
--Implementa Alumno con sintaxis de registro
data Alumno'' = Alum' {dni :: Int, expediente :: String, nota :: Float} deriving Show

--Apartado A:
aprobado'' :: Alumno'' -> Bool
aprobado'' (Alum' _ _ n) = if n >= 5 then True else False

--Apartado B:
calificacionAlumno'' :: Alumno'' -> String
calificacionAlumno'' (Alum' _ e n) = "Expediente: " ++ show e ++ " - Nota acta: " ++ show n

--Apartado C:
dameNota'' :: Alumno'' -> NotaPD
dameNota'' (Alum' _ _ n) = n

--Apartado D:
mediaNotas'' :: [Alumno''] -> Float
mediaNotas'' [] = 0
mediaNotas'' (x:xs) = (dameNota'' x + mediaNotas'' xs) / 2

--Tema 5 - Listado 1 - Ejercicio 5
data Complejo = Com Float Float deriving Show 

--Apartado A:
parteReal :: Complejo -> Float
parteReal (Com a _) = a

--Apartado B:
sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos (Com a b) (Com c d) = Com (a + c) (b + d)

--Tema 5 - Listado 1 - Ejercicio 6
--Con type
type Numerador = Integer
type Denominador = Integer

type Racional = (Numerador, Denominador)

aux :: Racional -> Racional -> Bool
aux (a,b) (c, d) = if a*d == b*c then True else False

equivalentes :: Racional -> [Racional] -> [Racional]
equivalentes n xs = [y | y <- xs, aux n y]

--Con data
data Racional' = R Numerador Denominador deriving Show

aux' :: Racional' -> Racional' -> Bool
aux' (R a b) (R c d) = if a*d == b*c then True else False

equivalentes' :: Racional' -> [Racional'] -> [Racional']
equivalentes' n xs = [y | y <- xs, aux' n y]



