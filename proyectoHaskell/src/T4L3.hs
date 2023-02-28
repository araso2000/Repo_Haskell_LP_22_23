module T4L3 where

import Data.List
import Data.Char

--Tema 4 - Listado 3 - Ejercicio 1
mezclarEnTernas :: [a] -> [b] -> [(a, b, b)]
mezclarEnTernas [] _ = []
mezclarEnTernas _ [] = []
mezclarEnTernas (x:xs) (y1:y2:ys) = (x,y1,y2) : mezclarEnTernas xs ys
mezclarEnTernas (x:xs) (y:[]) = [(x,y,y)]
mezclarEnTernas (x:[]) (y1:y2:ys) = [(x,y1,y2)]
mezclarEnTernas (x:[]) (y:[]) = [(x,y,y)]

--Tema 4 - Listado 3 - Ejercicio 2
--Normal
alFinal :: a -> [a] -> [a]
alFinal n xs = xs ++ [n]

--Con foldr
alFinal' :: a -> [a] -> [a]
alFinal' n xs = foldr (\x acc -> x : acc) [n] xs

--Tema 4 - Listado 3 - Ejercicio 3
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f xs = foldr (\x acc -> if f x then x : acc else []) [] xs

--Tema 4 - Listado 3 - Ejercicio 4
--posicionesElem :: (a, b) -> [c]
--posicionesElem (x,xs) = snd $ foldl (\(i, acc) y -> if y == x then (i+1, acc ++ [i]) else (i+1, acc)) (0, []) xs

--Tema 4 - Listado 3 - Ejercicio 5
contiene :: Eq a => a -> [a] -> Bool
contiene x ys = foldr (\y acc -> x == y || acc) False ys

contiene' :: Eq a => a -> [a] -> Bool
contiene' x ys = foldl (\acc y -> x == y || acc) False ys