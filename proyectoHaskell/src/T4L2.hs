module T4L2 where

import Data.List
import Data.Char

--Tema 4 - Listado 2 - Ejercicio 1
doble :: Int -> Int
doble x = x + x

doble' :: Int -> Int
doble' x = (\x -> x*2) x

--Tema 4 - Listado 2 - Ejercicio 2
sumaDobles :: [Int] -> Int
sumaDobles lista = foldl (+) 0 (map (*2) lista)

--Recursividad final:
sumaDobles' :: [Int] -> Int
sumaDobles' xs = sumaDobles'aux xs 0
            where  
                sumaDobles'aux [] acc = acc
                sumaDobles'aux (x:xs) acc = sumaDobles'aux xs (acc + 2*x)

--Recursividad no final
sumaDobles'' :: [Int] -> Int
sumaDobles'' [] = 0
sumaDobles'' (x:xs) = 2*x + sumaDobles'' xs

--Tema 4 - Listado 2 - Ejercicio 3
sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares lista = foldl (+) 0 (map (^2) (filter even lista))

sumaCuadradosPares' :: [Int] -> Int
sumaCuadradosPares' lista = foldl (+) 0 [y^2 | y <- lista, even y]

--Tema 4 - Listado 2 - Ejercicio 4
--Con foldr
eliminaValor :: Int -> [Int] -> [Int]
eliminaValor n xs = foldr (\x acc -> if x == n then acc else x : acc) [] xs

--Con foldl
eliminaValor' :: Int -> [Int] -> [Int]
eliminaValor' n xs = foldl (\acc x -> if x == n then acc else acc ++ [x]) [] xs

--Tema 4 - Listado 2 - Ejercicio 5
eliminaDuplicados :: [Int] -> [Int]
eliminaDuplicados xs = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) [] xs

--Tema 4 - Listado 2 - Ejercicio 6
divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], x `mod` y == 0]

esPrimo :: Int -> Bool
esPrimo x = if length(divisores x) == 2 || length(divisores x) == 1 then True else False

--Usando filter
listaPrimos :: [Int] -> [Int]
listaPrimos xs = filter esPrimo xs

--Usando recursividad
listaPrimos' :: [Int] -> [Int]
listaPrimos' [] = []
listaPrimos' (x:xs) 
                    | esPrimo x = x : listaPrimos' xs
                    | otherwise = listaPrimos' xs

