module Lib where
import Data.Char

celsius :: Int -> Int
celsius x = (x - 32) * 5 `div` 9

doble :: Int -> Int
doble x = 2*x

suma :: Int -> Int -> Int
suma x y = x + y

composicion :: Int->Int->Int
composicion x = (suma x).doble
--Primero se ejecutaria DOBLE con el numero 2 y luego se sumaria ese doble (que seria 4) a 1, es decir, 5

compCuatro :: Int -> Int -> Int
compCuatro x = (suma x).negate.(suma x).doble

divEntera :: (Int, Int) -> (Int, Int)
divEntera (m,n) = (m `div` n, m `rem` n)

--Definir una función que dados dos intervalos de numeros enteros diga si hay intersección entre ellos
interseccion :: (Int, Int) -> (Int,Int) -> Bool
interseccion (m,n) (x,y) = not (n < x)

--Definir una funcion que dados tres numeros enteros devuelva el mayor de ellos. Definir la funcion con guardas
mayorDe3 :: Int -> Int -> Int -> Int
mayorDe3 x y z
            | x > y && y > z = x
            | y > z && z > x = x
            | x > y && y > z = x

--Definir una funcion que dados tres numeros enteros devuelva el mayor de ellos. Se debe utilizar en la definicion de 
--esta funcion otra funcion que calcule el maximo de dos numeros enteros
maximoDos :: Int -> Int -> Int
maximoDos x y = if x >= y then x else y

mayorDeTres :: Int -> Int -> Int -> Int
mayorDeTres x y = maximoDos x.maximoDos y

mayorDeTres' :: Int -> Int -> Int -> Int
mayorDeTres' x = maximoDos.maximoDos x


--Definir una funcion que dados dos numeros devuelva una lista con el resultado de sumar, restar y multiplicar
--ambos numeros
listaOperaciones :: Int -> Int -> [Int]
listaOperaciones x y = [x+y, x-y, x*y]

--Definir una funcion que dadas dos cadenas de caracteres, si ambos tienen una longitud menor o igual a tres devolvera
--su union, en otro caso devolvera una cadena vacia
longCadenas :: [Char] -> [Char] -> [Char]
longCadenas x y = if length x <= 3 && length y <=3 then x ++ y else [ ]
--Tambien se puede hacer con String, que soy bobo

--Definir una funcion que dada una frase devuelva una cadena formada unicamente por los caracteres en mayusculas
listaMayus :: String -> String
listaMayus x = [y | y <- x, isUpper y]

abecedario :: String
abecedario = [x | x <- "El perro de San Roque no tiene rabo", isUpper x]

--(cabeza:resto) esto es una lista
--[cabeza:resto] esto es una lista de listas
--[x,y] lista limitada segun los elementos que indiquemos como variables

--Dada una funcion que dada una lista de caracteres indique si su longitud es menor o igual a tres elemento. No se puede utilizar recursividad, ni funciones de longitud predefinidas
longMenorTres :: String -> Bool
longMenorTres [] = True
longMenorTres [x] = True
longMenorTres [x,y] = True
longMenorTres _ = True

longMenorTres' :: String -> Bool
longMenorTres' (c1:c2:c3:c4:cs) = False
longMenorTres' _ = True

--Definir una funcion que dada una cadena de caracteres indique si comienza o no por mayuscula
primeraMayus :: String -> Bool
primeraMayus (x:xs) = x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

primeraMayus' :: String -> Bool
primeraMayus' (x:_) = isUpper x
primeraMayus' _ = False

--Definir una funcion que dadas dos cadenas de carateres las devuelva ordenadas alfabeticamente segun su primer caracter. Sin recursividad, si una cadena está vacía, irá en primer lugar
ordenarAlfabeto :: String -> String -> (String,String)
ordenarAlfabeto lista [] = ([],lista)
ordenarAlfabeto [] lista = ([], lista)
ordenarAlfabeto c1@(x:xs) c2@(y:ys) = if (toLower x) < (toLower y) then (c1,c2) else (c2,c1)

--Definir una funcion recursiva que dadas una cadena y un caracter, cuente el numero de apariciones del caracter en cadena
countChar :: Char -> String -> Int
countChar _ [] = 0  -- Caso base: la cadena es vacía, no hay apariciones del caracter
countChar c (x:xs)
                    | c == x    = 1 + countChar c xs  -- El primer caracter de la cadena es igual al buscado, sumamos 1 y seguimos buscando en el resto
                    | otherwise = countChar c xs     -- El primer caracter de la cadena es diferente al buscado, seguimos buscando en el resto

--Definir una funcion recursiva que dada una lista de numeros enteros devuelva como resultado la suma de todos ellos
sumaNumeros :: [Int] -> Int
sumaNumeros [] = 0
sumaNumeros (x:xs) = x + sumaNumeros xs

sumaNumerosTail :: [Int] -> Int
sumaNumerosTail lista = sumaTail 0 lista
    where
        sumaTail :: Int -> [Int] -> Int
        sumaTail res [] = res
        sumaTail res (x:xs) = sumaTail (res + x) xs


--Foldr recorre de derecha a izquierda y foldl de izquierda a derecha la lista dada
-- foldr = (w + (x + (y + (z + e) ) ) )
-- foldl = ( ( ( (w + x) + y) + z) + e)

--Tema 3 - Listado 0 - Ejercicio 1 
componer :: Int -> Int -> Int
componer x y = maximum [div x y, mod x y]

--Tema 3 - Listado 0 - Ejercicio 2
sucesor :: Int -> Int
sucesor x = x + 1

--Tema 3 - Listado 0 - Ejercicio 3
cuadruple :: Int -> Int
cuadruple x = 2 * doble x

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