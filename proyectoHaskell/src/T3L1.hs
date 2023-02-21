module T3L1 where
import Data.Char

--Tema 3 - Listado 1 - Ejercicio 1
ordenadosMenor :: Int -> Int -> Int -> Bool
ordenadosMenor x y z = if x<y && y<z then True else False

--Tema 3 - Listado 1 - Ejercicio 2
--ordenarTupla :: (Int, Int, Int) -> (Int, Int, Int)
--ordenarTupla (x, y, z) = (a, b, c)
--  where
--    [a, b, c] = sort [x, y, z]

--Tema 3 - Listado 1 - Ejercicio 3
descomponerReal :: Float -> (Int,Int)
descomponerReal x = (truncate x, (round (x * 100) `mod` 100))

--Tema 3 - Listado 1 - Ejercicio 4
divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], x `mod` y == 0]

--Tema 3 - Listado 1 - Ejercicio 5
esDigito :: Char -> Bool
esDigito '0' = True
esDigito '1' = True
esDigito '2' = True
esDigito '3' = True
esDigito '4' = True
esDigito '5' = True
esDigito '6' = True
esDigito '7' = True
esDigito '8' = True
esDigito '9' = True
esDigito _ = False

esDigito' :: Char -> Bool
esDigito' x 
            | (digitToInt x) >=0 && (digitToInt x) <=9 = True
            | otherwise = False

--Tema 3 - Listado 1 - Ejercicio 6
esPrimo :: Int -> Bool
esPrimo x = if length(divisores x) == 2 || length(divisores x) == 1 then True else False

--Tema 3 - Listado 1 - Ejercicio 7
listaPrimosImpares :: [Int] -> [Int]
listaPrimosImpares x = [y | y <- x, not (even y) && esPrimo y]

--Tema 3 - Listado 1 - Ejercicio 8
primosMenorIgual :: Int -> [Int]
primosMenorIgual x = [y | y <- [1..x], y<=x && esPrimo y]

--Tema 3 - Listado 1 - Ejercicio 9
codificacionTuplas :: [(Char,Char)] -> String
codificacionTuplas mensaje = [c1 | (c1,c2) <- mensaje, c2 `elem` "aeiouAEIOU"]

--Tema 3 - Listado 1 - Ejercicio 10
filtrarTuplas :: Int -> [(Int,Int)] -> [(Int,Int)]
filtrarTuplas n lista = [(c1,c2) | (c1,c2) <- lista, (not (even c1)) && c1>n]

--Tema 3 - Listado 1 - Ejercicio 11
cuantasPitagoricas :: [(Int,Int,Int)] -> Int
cuantasPitagoricas lista = length([(c1,c2,c3) | (c1,c2,c3) <- lista, (c1*c1 + c2*c2) == c3*c3])

--Tema 3 - Listado 1 - Ejercicio 12
esMayuscula :: Char -> Bool
esMayuscula x = x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

--Tema 3 - Listado 1 - Ejercicio 13
mayusculasMinusculas :: String -> String
mayusculasMinusculas lista = [if isUpper c then toLower c else toUpper c | c <- lista]

--Tema 3 - Listado 1 - Ejercicio 14
listaASCII :: String -> [Int]
listaASCII lista = [ord x | x <- lista]

--Tema 3 - Listado 1 - Ejercicio 15
mensajeLista :: [Int] -> String
mensajeLista (x:xs) = "Primer elemento: " ++ show x ++ ", longitud: " ++ show (length (x:xs))

--Tema 3 - Listado 1 - Ejercicio 16
contarMayusculas :: String -> Int
contarMayusculas lista = length([x | x <- lista, esMayuscula x])