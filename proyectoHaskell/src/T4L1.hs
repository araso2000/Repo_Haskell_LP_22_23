module T4L1 where

import Data.List
import Data.Char

--Tema 4 - Listado 1 - Ejercicio 1
cribar :: [Int] -> Int -> [Int]
cribar lista n = [y | y <- lista, y `mod` n /= 0]

cribar' :: [Int] -> Int -> [Int]
cribar' [] _ = []
cribar' (y:ys) n = if y `mod` n == 0 then cribar' ys n else y : cribar' ys n

cribar'' :: [Int] -> Int -> [Int]
cribar'' [] _ = []
cribar'' (y:ys) n 
            | y `mod` n == 0 = cribar'' ys n
            | otherwise = y : cribar'' ys n

--Tema 4 - Listado 1 - Ejercicio 2
ceros :: [Int] -> Int
ceros [] = 0
ceros (x:xs)
            | x == 0 = 1 + ceros (dropWhile (== 0) xs)
            | otherwise = ceros xs

--Tema 4 - Listado 1 - Ejercicio 3
repeticiones :: [Int] -> ([Int],[Int])
repeticiones [] = ([],[])
repeticiones (x:xs) = let(normal, repetidos) = repeticiones xs in if elem x normal then (normal, x : repetidos) else (x : normal, repetidos)

--Tema 4 - Listado 1 - Ejercicio 4
incluye :: [Int] -> [Int] -> Bool
incluye xs ys = if length([y | y <- xs, y `elem` ys]) == length(xs) then True else False

incluye' :: [Int] -> [Int] -> Bool
incluye' [] _ = True --Primera lista vacia está contenida si o si en la segunda lista
incluye' _ [] = False --Si la segunda lista esta vacia, no puede contener nada de la primera lista
incluye' xs ys@(y:ys')
                    | prefijoDe xs ys = True --Si la primera lista es un prefijo de la segunda, estará contenida pues
                    | otherwise = incluye' xs ys' --Si no, probamos con la segunda lista

prefijoDe :: [Int] -> [Int] -> Bool
prefijoDe [] _ = True
prefijoDe _ [] = False
prefijoDe (x:xs) (y:ys)
                    | x == y = prefijoDe xs ys --si los primeros elementos coinciden, seguimos verificando
                    | otherwise = False --si no, la primera lista no será un prefijo de la segunda

--Tema 4 - Listado 1 - Ejercicio 5 ???
sumaCifras :: Int -> Int
sumaCifras n = foldl (+) 0 (obtenerCifras n)

obtenerCifras :: Int -> [Int]
obtenerCifras n = map digitToInt (show n)

--Tema 4 - Listado 1 - Ejercicio 6
contieneCifra :: Int -> Int -> Bool
contieneCifra n xs = n `elem` (obtenerCifras xs)

--Tema 4 - Listado 1 - Ejercicio 7
invertir :: Int -> Int
invertir n = listaNumero (foldr (\x acc -> acc ++ [x]) [] (obtenerCifras n))

listaNumero :: [Int] -> Int
listaNumero xs = foldl (\y x -> y * 10 + x) 0 xs

--Tema 4 - Listado 1 - Ejercicio 8
eliminarUltimos :: Int -> [Int] -> [Int]
eliminarUltimos n xs = take (length(xs) - n) xs

eliminarUltimos' :: Int -> [Int] -> [Int]
eliminarUltimos' 0 xs = xs
eliminarUltimos' n [] = []
eliminarUltimos' n (x:xs) = eliminarUltimos' (n-1) xs

--Tema 4 - Listado 1 - Ejercicio 9
listaOrdenada :: [Int] -> Bool
listaOrdenada [] = True
listaOrdenada [_] = True
listaOrdenada (x1:x2:xs) = x1 < x2 && listaOrdenada (x2:xs)