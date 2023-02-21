module T3L0 where
import Data.Char

--Tema 3 - Listado 0 - Ejercicio 1 
componer :: Int -> Int -> Int
componer x y = maximum [div x y, mod x y]

--Tema 3 - Listado 0 - Ejercicio 2
sucesor :: Int -> Int
sucesor x = x + 1

--Tema 3 - Listado 0 - Ejercicio 3
doble :: Int -> Int
doble x = 2*x

cuadruple :: Int -> Int
cuadruple x = 2 * doble x